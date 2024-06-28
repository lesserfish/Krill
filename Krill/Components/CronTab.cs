#nullable enable
using System.Text.RegularExpressions;
using System.Collections.Generic;
using System.Linq;
using System;
namespace Nez.Extension;

public class CronException : Exception {
    public CronException(string message) : base(message){}
}

public class CronField {
    private CronField(int maximum){
        _times = new List<int>();
    }
    public static CronField Parse(string rule, int maximum){
        Match result;

        result = Regex.Match(rule, @"^\*$");
        if(result.Success){
            return CronField.FromAlways(maximum);
        }

        result = Regex.Match(rule, @"^(\d+)-(\d+)$");
        if(result.Success){
            int start = Int32.Parse(result.Groups[1].Value);
            int end = Int32.Parse(result.Groups[2].Value);
            return CronField.FromRange(maximum, start, end);
        }

        result = Regex.Match(rule, @"^(\d+|\*)\/(\d+)$");
        if(result.Success){
            int start = 0;
            if(result.Groups[1].Value != "*"){
                start = Int32.Parse(result.Groups[1].Value);
            }
            int step = Int32.Parse(result.Groups[2].Value);
            return CronField.FromStep(maximum, start, step);
        }

        result = Regex.Match(rule, @"^(\d+)(,\d+)*$");
        if(result.Success){
            List<int> values = new List<int>();

            MatchCollection matches = Regex.Matches(rule, @"\d+");

            foreach(Match match in matches) {
                int value = Int32.Parse(match.Value);
                values.Add(value);
            }

            return CronField.FromList(maximum, values);
        }

        throw new ArgumentException("Failed to parse CronField");

    }
    public static CronField FromAlways(int maximum){
        CronField field = new CronField(maximum);
        for(int i = 0; i < maximum; i++){
            field._times.Add(i);
        }
        return field;
    }
    public static CronField FromList(int maximum, List<int> values){
        CronField field = new CronField(maximum);
        field._times = values;
        return field;
    }
    public static CronField FromRange(int maximum, int start, int end){
        if(start > end || start >= maximum || end >= maximum){
            throw new ArgumentException("Incorrect arguments given to CronField");
        }
        CronField field = new CronField(maximum);
        for(int i = start; i <= end; i++){
            field._times.Add(i);
        } 
        return field;

    }
    public static CronField FromStep(int maximum, int start, int step){
        if(start >= maximum){
            throw new ArgumentException("Incorrect arguments given to CronField");
        }

        CronField field = new CronField(maximum);

        for(int i = 0; start + step*i < maximum; i++){
            field._times.Add(start + step * i);
        }

        return field;
    }
    public List<int> ClockSort(int value){
        List<int> Tail = new List<int>();
        List<int> Init = new List<int>();

        foreach(int candidate in _times){
            if(candidate < value){
                Tail.Add(candidate);
            } else {
                Init.Add(candidate);
            }
        }

        Init.Concat(Tail);
        return Init;
    }

    private List<int> _times;
};

public class CronSchedule {
    private CronSchedule(string decisecondRule, string secondRule, string minuteRule, string hourRule){
        _decisecondField = CronField.Parse(decisecondRule, 10);
        _secondField = CronField.Parse(secondRule, 60);
        _minuteField = CronField.Parse(minuteRule, 60);
        _hourField = CronField.Parse(hourRule, 100);
    }
    public static CronSchedule Create(string rule){
        string[] parts = rule.Split(' ');
        if(parts.Length != 4){
            throw new ArgumentException("Could not parse cron rule");
        }
        return new CronSchedule(parts[0], parts[1], parts[2], parts[3]);
    }
    public static CronSchedule Create(string decisecondRule, string secondRule, string minuteRule, string hourRule){
        return new CronSchedule(decisecondRule, secondRule, minuteRule, hourRule);
    }

    private static (int, int, int, int) FromSeconds(float time){
        int deciseconds = (int)Math.Floor(time * 10) % 10;
        int seconds = (int)Math.Floor(time) % 60;
        int minutes = (int)Math.Floor(time / 60) % 60;
        int hours = (int)Math.Floor(time / 3600) % 100;
        return (deciseconds, seconds, minutes, hours);
    }
    private static float ToSeconds(int deciseconds, int seconds, int minutes, int hours){
        float f1 = (float)(deciseconds) / 10;
        float f2 = (float) seconds;
        float f3 = (float) minutes * 60;
        float f4 = (float) hours * 3600;
        return f1 + f2 + f3 + f4;
    }

    public float NextTrigger(float currentTime){

        var (currentDecisecond, currentSecond, currentMinute, currentHour) = FromSeconds(currentTime);

        var hourCandidates = _hourField.ClockSort(currentHour);
        foreach(int hour in hourCandidates){
            var targetMinute = hour == currentHour ? currentMinute : -1;
            var minuteCandidates = _minuteField.ClockSort(targetMinute);

            foreach(int minute in minuteCandidates){
                var targetSecond = minute == currentMinute ? currentSecond : -1;
                var secondCandidates = _secondField.ClockSort(targetSecond);

                foreach(int second in secondCandidates) {
                    var targetDecisecond = second == currentSecond ? currentDecisecond : -1;
                    var decisecondCandidates = _decisecondField.ClockSort(targetDecisecond);

                    foreach (int decisecond in decisecondCandidates){

                        var targetTime = ToSeconds(decisecond, second, minute, hour);
                        var fixedTime= ToSeconds(currentDecisecond, currentSecond,currentMinute, currentHour);

                        if(targetTime > fixedTime) {
                            return targetTime;
                        }
                    }

                }
            }
        }
        throw new CronException("Could not find a suitable time for next event.");
    }

    private CronField _decisecondField;
    private CronField _secondField;
    private CronField _minuteField;
    private CronField _hourField;
}

public class CronJob : IComparable {
    public CronJob(Action job, string rule, bool useSceneTime = false, float currentTime = -1) {
        if(currentTime < 0){
            if(useSceneTime){
                currentTime = Nez.Time.TimeSinceSceneLoad;
            } else {
                currentTime = Nez.Time.TotalTime;
            }
        }
        Job = job;
        schedule = CronSchedule.Create(rule);
        NextTrigger = schedule.NextTrigger(currentTime);
        _useSceneTime = useSceneTime;
    }
    public CronJob(
            Action job, 
            string decisecondRule = "", 
            string secondRule = "", 
            string minuteRule = "", 
            string hourRule = "", 
            bool useSceneTime = false, 
            float currentTime = -1
    ){

        if(currentTime < 0){
            if(useSceneTime){
                currentTime = Nez.Time.TimeSinceSceneLoad;
            } else {
                currentTime = Nez.Time.TotalTime;
            }
        }
        Job = job;
        schedule = CronSchedule.Create(decisecondRule, secondRule, minuteRule, hourRule);
        NextTrigger = schedule.NextTrigger(currentTime);
        _useSceneTime = useSceneTime;
    }

    public void UpdateTrigger(float currentTime = -1){
        if(currentTime < 0){
            if(_useSceneTime){
                currentTime = Nez.Time.TimeSinceSceneLoad;
            } else {
                currentTime = Nez.Time.TotalTime;
            }
        }
        NextTrigger = schedule.NextTrigger(currentTime);
    }

    public void Trigger(){
        Job();
    }
    
    public float TimeToTrigger(float currentTime = -1) {
        if(currentTime < 0){
            if(_useSceneTime){
                currentTime = Nez.Time.TimeSinceSceneLoad;
            } else {
                currentTime = Nez.Time.TotalTime;
            }
        }

        return NextTrigger - currentTime;
    }


    public int CompareTo(object? other){
        if(other is null){
            throw new NullReferenceException("Attempted to compare to a null value");
        }
        if(!(other is CronJob)){
            throw new ArgumentException("Invalid comparison to a CronJob");
        }
        CronJob cother = (other as CronJob)!;

        return NextTrigger.CompareTo(cother.NextTrigger);
    }
    public Action Job {get; private set;}
    public CronSchedule schedule {get; private set;}
    public float NextTrigger {get; private set;}
    private bool _useSceneTime;
}


public class Cron {

    class TaggedJob : IComparable {
        public TaggedJob(CronJob job, long id) {
            Job = job;
            Id = id;
        }
        public void Trigger(){
            Job.Trigger();
            Job.UpdateTrigger();
        }
        public float TimeToTrigger() {
            return Job.TimeToTrigger();
        }
        public int CompareTo(object? other){
            if(other == null){
                throw new ArgumentException("Attempted to compare TaggedJob to null");
            }
            if(!(other is TaggedJob)){
                throw new ArgumentException("Comparison between unsupported types");
            }

            TaggedJob tother = (other as TaggedJob)!;
            return Job.CompareTo(tother.Job);
        }
        public CronJob Job;
        public long Id;
    }

    public static Cron? Instance;
    public static void Update(Cron? instance = null){
        if(instance == null){
            instance = Instance;
        }

        if(instance == null){
            throw new NullReferenceException("CronScheduler has not been initialized");
        }
        instance._update();
    }
    public static long AddJob(CronJob job, Cron? instance = null){
        if(instance == null){
            instance = Instance;
        }

        if(instance == null){
            throw new NullReferenceException("CronScheduler has not been initialized");
        }

        return instance._addJob(job);
    }
    public static void RemoveJob(long id, Cron? instance = null){
        if(instance == null){
            instance = Instance;
        }

        if(instance == null){
            throw new NullReferenceException("CronScheduler has not been initialized");
        }
        instance._removeJob(id);
    }

    public Cron() {
        _jobList = new List<TaggedJob>();
        _id = 0;
    }

    private void _update(){
        bool sort = false;
        foreach(TaggedJob job in _jobList){
            if(job.TimeToTrigger() <= 0){
                job.Trigger();
                sort = true;
                continue;
            }
            break;
        }

        if(sort){
            _jobList.Sort();
        }
    }

    private long _addJob(CronJob job){
        long jobId = ++_id;
        TaggedJob tjob = new TaggedJob(job, jobId);
        _jobList.Add(tjob);
        _jobList.Sort();
        return jobId;
    }
    private void _removeJob(long id){
        _jobList.RemoveAll(j => j.Id == id);
    }
    private List<TaggedJob> _jobList;
    private long _id;
}


public class CronTab : Nez.Component {
    public CronTab(){
        _jobIds = new Dictionary<string, long>();
    }
    public void RegisterJob(
            string jobName,             
            Action jobAction,
            string decisecondRule, 
            string secondRule, 
            string minuteRule, 
            string hourRule, 
            bool useSceneTime = false)
    {
        if(_jobIds.ContainsKey(jobName)){
            throw new ArgumentException("A cron job already exists with this name");
        }
        long jobId = Cron.AddJob(new CronJob(jobAction, decisecondRule, secondRule, minuteRule, hourRule, useSceneTime));
        _jobIds.Add(jobName, jobId);

    }
    public void RegisterJob(string jobName, Action jobAction, string rule, bool useSceneTime = false)
    {
        if(_jobIds.ContainsKey(jobName)){
            throw new ArgumentException("A cron job already exists with this name");
        }
        long jobId = Cron.AddJob(new CronJob(jobAction, rule, useSceneTime));
        _jobIds.Add(jobName, jobId);

    }
    public void RemoveJob(string jobName){
        if(!_jobIds.ContainsKey(jobName)){
            throw new ArgumentException("");
        }
        long jobId = _jobIds[jobName];
        Cron.RemoveJob(jobId);
        _jobIds.Remove(jobName);
    }

    public override void OnRemovedFromEntity(){
        foreach (var (jobName, jobId) in _jobIds){
            Cron.RemoveJob(jobId);
        }
        _jobIds.Clear();
    }

    private Dictionary<string, long> _jobIds;
}
