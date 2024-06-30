using Nez.Extension;

public class CT {
    [Fact]
    public void Unit1() {
        float currentTime = 1560.4500f;
        float targetTime = CronSchedule.Create("* * * *").NextTrigger(currentTime); // Every decisecond

        Assert.Equal(1560.5f, targetTime);
    }

    [Fact]
    public void Unit2(){
        float currentTime = 1000.4500f;
        float targetTime = CronSchedule.Create("0 * * *").NextTrigger(currentTime); // Every second

        Assert.Equal(1001.0f, targetTime);
    }
    [Fact]
    public void Unit3(){
        float currentTime = 50.0500f;
        float targetTime = CronSchedule.Create("* 1-3 * *").NextTrigger(currentTime); // At seconds 1, 2, 3 of every minute

        Assert.Equal(61.0f, targetTime);
    }
    [Fact]
    public void Unit4(){
        float currentTime = 65.0500f;
        float targetTime = CronSchedule.Create("0 0 * *").NextTrigger(currentTime); // Every minute

        Assert.Equal(120.0f, targetTime);
    }
    [Fact]
    public void Unit5(){
        float currentTime = 60.0500f;
        float targetTime = CronSchedule.Create("0 0 * *").NextTrigger(currentTime); // Every minute

        Assert.Equal(120.0f, targetTime);
    }
    [Fact]
    public void Unit6(){
        float currentTime = 65.0500f;
        float targetTime = CronSchedule.Create("0 20/5 * *").NextTrigger(currentTime); // Every 5 seconds, starting from second 20 to 59

        Assert.Equal(80.0f, targetTime);
    }

    [Fact]
    public void Unit7(){
        float currentTime = 83.0500f;
        float targetTime = CronSchedule.Create("0 20/5 * *").NextTrigger(currentTime); // Every 5 seconds, starting from second 20 to 59

        Assert.Equal(85.0f, targetTime);
    }

    [Fact]
    public void Unit8(){
        float currentTime = 60.0500f;
        float targetTime = CronSchedule.Create("0 2,8 * *").NextTrigger(currentTime); // At seconds 2 and 8, every minute

        Assert.Equal(62.0f, targetTime);
    }

    [Fact]
    public void Unit9(){
        float currentTime = 63.0500f;
        float targetTime = CronSchedule.Create("0 2,8 * *").NextTrigger(currentTime); // At seconds 2 and 8, every minute

        Assert.Equal(68.0f, targetTime);
    }

    [Fact]
    public void Unit10(){
        float currentTime = 70.0500f;
        float targetTime = CronSchedule.Create("0 2,8 * *").NextTrigger(currentTime); // At seconds 2 and 8, every minute

        Assert.Equal(122.0f, targetTime);
    }
    [Fact]
    public void Unit11(){
        float currentTime = 70.0500f;
        float targetTime = CronSchedule.Create("0 2,8 * 1").NextTrigger(currentTime); // At seconds 2 and 8, every hour

        Assert.Equal(3602.0f, targetTime);
    }
    [Fact]
    public void Unit12(){
        float currentTime = 83.0500f;
        float targetTime = CronSchedule.Create("0 0 20/5 *").NextTrigger(currentTime); // Every 5 minutes,starting from minute 20 to 59

        Assert.Equal(1200.0f, targetTime);
    }

    [Fact]
    public void Unit13(){
        float currentTime = 60.0500f;
        float targetTime = CronSchedule.Create("0 0 2,8 *").NextTrigger(currentTime); // At minutes 2 and 8, every hour

        Assert.Equal(120.0f, targetTime);
    }
    [Fact]
    public void Unit14(){
        float currentTime = 125.0500f;
        float targetTime = CronSchedule.Create("0 0 2,8 *").NextTrigger(currentTime); // At minutes 2 and 8, every hour

        Assert.Equal(480.0f, targetTime);
    }



}
