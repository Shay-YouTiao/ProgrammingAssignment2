using System.Collections.Generic;
using System.Text.Json.Serialization;

namespace HmiParser
{
    // JSON序列化的数据模型
    public class HmiProject
    {
        [JsonPropertyName("namespace")]
        public string Namespace { get; set; }

        [JsonPropertyName("using")]
        public string Using { get; set; }

        [JsonPropertyName("project")]
        public ViewProject Project { get; set; }
    }

    public class ViewProject
    {
        [JsonPropertyName("name")]
        public string Name { get; set; }

        [JsonPropertyName("homescreen")]
        public string HomeScreen { get; set; }

        [JsonPropertyName("screens")]
        public List<HmiObject> Screens { get; set; } = new List<HmiObject>();

        [JsonPropertyName("popups")]
        public List<HmiObject> Popups { get; set; } = new List<HmiObject>();

        [JsonPropertyName("addOnGraphics")]
        public List<HmiObject> AddOnGraphics { get; set; } = new List<HmiObject>();

        [JsonPropertyName("banners")]
        public List<HmiObject> Banners { get; set; } = new List<HmiObject>();

        [JsonPropertyName("dataLogs")]
        public List<HmiObject> DataLogs { get; set; } = new List<HmiObject>();

        [JsonPropertyName("shortcuts")]
        public List<HmiObject> Shortcuts { get; set; } = new List<HmiObject>();

        [JsonPropertyName("folders")]
        public List<HmiObject> Folders { get; set; } = new List<HmiObject>();
    }

    public class HmiObject
    {
        [JsonPropertyName("type")]
        public string Type { get; set; }

        [JsonPropertyName("name")]
        public string Name { get; set; }

        [JsonPropertyName("properties")]
        public Dictionary<string, object> Properties { get; set; } = new Dictionary<string, object>();

        [JsonPropertyName("children")]
        public List<HmiObject> Children { get; set; } = new List<HmiObject>();

        [JsonPropertyName("events")]
        public List<HmiObject> Events { get; set; } = new List<HmiObject>();

        [JsonPropertyName("behaviors")]
        public List<HmiObject> Behaviors { get; set; } = new List<HmiObject>();

        [JsonPropertyName("stateTables")]
        public List<HmiObject> StateTables { get; set; } = new List<HmiObject>();

        [JsonPropertyName("securityRoles")]
        public Dictionary<string, string> SecurityRoles { get; set; } = new Dictionary<string, string>();

        [JsonPropertyName("userProperties")]
        public List<HmiObject> UserProperties { get; set; } = new List<HmiObject>();
    }

    public class PropertyValue
    {
        [JsonPropertyName("value")]
        public string Value { get; set; }

        [JsonPropertyName("bound")]
        public string Bound { get; set; }
    }

    public class StateTable
    {
        [JsonPropertyName("expression")]
        public PropertyValue Expression { get; set; }

        [JsonPropertyName("states")]
        public List<State> States { get; set; } = new List<State>();
    }

    public class State
    {
        [JsonPropertyName("name")]
        public string Name { get; set; }

        [JsonPropertyName("properties")]
        public Dictionary<string, string> Properties { get; set; } = new Dictionary<string, string>();

        [JsonPropertyName("value")]
        public string Value { get; set; }
    }
}