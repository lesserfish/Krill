#nullable enable
using System;
using System.IO;
using Microsoft.Xna.Framework;
using Microsoft.Xna.Framework.Graphics;
using Nez.Systems;

namespace Nez.Extension {

    class Loader {

        public static Loader? Instance;

        private NezContentManager _nezContentManager;
        private string _rootDirectory;
        private string _modDirectory;
    }
}
