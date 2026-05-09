using System;
using System.Collections.Generic;
using System.Text.Json;
using System.Text.Json.Serialization;

namespace HmiParser
{
    /// <summary>
    /// HMI 对象基类
    /// </summary>
    public class HmiObject
    {
        public string Name { get; set; } = "";
        public Dictionary<string, object> Properties { get; set; } = new Dictionary<string, object>();
        public List<HmiObject> Children { get; set; } = new List<HmiObject>();
        public string ObjectType { get; set; } = "";

        public virtual Dictionary<string, object> ToDictionary()
        {
            var result = new Dictionary<string, object>
            {
                ["name"] = Name,
                ["type"] = ObjectType,
                ["properties"] = Properties
            };

            if (Children.Count > 0)
            {
                var childrenList = new List<Dictionary<string, object>>();
                foreach (var child in Children)
                {
                    childrenList.Add(child.ToDictionary());
                }
                result["children"] = childrenList;
            }

            return result;
        }

        public string ToJson(bool indented = true)
        {
            var options = new JsonSerializerOptions
            {
                WriteIndented = indented,
                DefaultIgnoreCondition = JsonIgnoreCondition.WhenWritingNull
            };
            return JsonSerializer.Serialize(ToDictionary(), options);
        }
    }

    /// <summary>
    /// 边框样式
    /// </summary>
    public class BorderStyle : HmiObject
    {
        public PenStyle Line { get; set; } = PenStyle.SolidLine;
        public CapStyle Cap { get; set; } = CapStyle.SquareCap;
        public JoinStyle Join { get; set; } = JoinStyle.MiterJoin;

        public BorderStyle()
        {
            ObjectType = "BorderStyle";
        }
    }

    /// <summary>
    /// 边框
    /// </summary>
    public class Border : HmiObject
    {
        public string Color { get; set; } = "#000000";
        public BorderStyle Style { get; set; } = new BorderStyle();
        public int Width { get; set; } = 1;

        public Border()
        {
            ObjectType = "Border";
        }
    }

    /// <summary>
    /// 图形元素基类
    /// </summary>
    public class GraphicElement : HmiObject
    {
        public double X { get; set; } = 0.0;
        public double Y { get; set; } = 0.0;
        public double Width { get; set; } = 0.0;
        public double Height { get; set; } = 0.0;
        public double Angle { get; set; } = 0.0;
        public bool Enabled { get; set; } = true;
        public bool Visible { get; set; } = true;
        public int Opacity { get; set; } = 100;
        public EnumElementAccessFamily Access { get; set; } = EnumElementAccessFamily.Inherit;
        public bool ForceAnimations { get; set; } = false;
        public string ElementType { get; set; } = "";

        public GraphicElement()
        {
            ObjectType = "GraphicElement";
        }
    }

    /// <summary>
    /// 按钮元素
    /// </summary>
    public class Button : GraphicElement
    {
        public string FillColor { get; set; } = "#263a4e";
        public Border Border { get; set; }
        public int CornerRadius { get; set; } = 3;
        public EnumAlignment TextAlignment { get; set; } = EnumAlignment.HCENTER_VCENTER;
        public string FontName { get; set; } = "Arial Unicode MS";
        public double FontSize { get; set; } = 12;
        public string FontColor { get; set; } = "#ffffff";
        public bool Bold { get; set; } = true;
        public string Text { get; set; } = "";
        public bool UsePredefinedDisabled { get; set; } = true;

        public Button()
        {
            ElementType = "Button";
            ObjectType = "Button";
        }
    }

    /// <summary>
    /// 文本显示元素
    /// </summary>
    public class TextDisplay : GraphicElement
    {
        public string Text { get; set; } = "";
        public string FontName { get; set; } = "Arial Unicode MS";
        public double FontSize { get; set; } = 15;
        public bool Bold { get; set; } = true;
        public bool Underline { get; set; } = false;
        public string FontColor { get; set; } = "#000000";
        public EnumAlignment TextAlignment { get; set; } = EnumAlignment.HLEFT_VCENTER;
        public int Padding { get; set; } = 0;
        public int CornerRadius { get; set; } = 0;
        public Border Border { get; set; }
        public string FillColor { get; set; } = "#00000000";

        public TextDisplay()
        {
            ElementType = "TextDisplay";
            ObjectType = "TextDisplay";
        }
    }

    /// <summary>
    /// 条形图元素
    /// </summary>
    public class BarGraph : GraphicElement
    {
        public string FillColor { get; set; } = "#4d4d4d";
        public int FillOpacity { get; set; } = 100;
        public int TickMarkOpacity { get; set; } = 100;
        public int TextOpacity { get; set; } = 100;
        public string LevelColor { get; set; } = "#29abe2";
        public object Value { get; set; } = 0;
        public double MinValue { get; set; } = 0;
        public double MaxValue { get; set; } = 100;
        public string HousingColor { get; set; } = "#333333";
        public string TickMarkColor { get; set; } = "#f2f2f2";
        public string FontName { get; set; } = "Arial Unicode MS";
        public double FontSize { get; set; } = 12;
        public string FontColor { get; set; } = "#ffffff";

        public BarGraph()
        {
            ElementType = "BarGraph1";
            ObjectType = "BarGraph1";
        }
    }

    /// <summary>
    /// 状态定义
    /// </summary>
    public class StateDefinition : HmiObject
    {
        public object Value { get; set; }

        public StateDefinition()
        {
            ObjectType = "State";
        }
    }

    /// <summary>
    /// 状态表
    /// </summary>
    public class StateTable : HmiObject
    {
        public string Expression { get; set; } = "";
        public List<StateDefinition> States { get; set; } = new List<StateDefinition>();

        public StateTable()
        {
            ObjectType = "StateTable";
        }
    }

    /// <summary>
    /// 屏幕
    /// </summary>
    public class Screen : HmiObject
    {
        public bool ShowDefaultBanner { get; set; } = true;
        public string FillColor { get; set; } = "#ffffff";
        public EnumUpdateRate UpdateRate { get; set; } = EnumUpdateRate._500_ms;
        public double Width { get; set; } = 1280;
        public double Height { get; set; } = 981;
        public bool ForceAnimations { get; set; } = false;
        public List<GraphicElement> GraphicElements { get; set; } = new List<GraphicElement>();
        public List<StateTable> StateTables { get; set; } = new List<StateTable>();
        public Dictionary<string, RoleAccess> SecurityRoles { get; set; } = new Dictionary<string, RoleAccess>();

        public Screen()
        {
            ObjectType = "Screen";
        }

        public override Dictionary<string, object> ToDictionary()
        {
            var result = base.ToDictionary();
            
            result["screenProperties"] = new Dictionary<string, object>
            {
                ["showDefaultBanner"] = ShowDefaultBanner,
                ["fillColor"] = FillColor,
                ["updateRate"] = UpdateRate.ToString(),
                ["width"] = Width,
                ["height"] = Height,
                ["forceAnimations"] = ForceAnimations
            };

            var elements = new List<Dictionary<string, object>>();
            foreach (var el in GraphicElements)
            {
                elements.Add(el.ToDictionary());
            }
            result["graphicElements"] = elements;

            if (SecurityRoles.Count > 0)
            {
                var roles = new Dictionary<string, string>();
                foreach (var kvp in SecurityRoles)
                {
                    roles[kvp.Key] = kvp.Value.ToString();
                }
                result["securityRoles"] = roles;
            }

            return result;
        }
    }

    /// <summary>
    /// 弹出窗口
    /// </summary>
    public class Popup : HmiObject
    {
        public bool CaptionVisible { get; set; } = true;
        public bool CloseButtonVisible { get; set; } = true;
        public string Caption { get; set; } = "Popup Caption";
        public EnumUpdateRate UpdateRate { get; set; } = EnumUpdateRate._500_ms;
        public string FillColor { get; set; } = "#263a4e";
        public int BackgroundOpacity { get; set; } = 100;
        public bool Cacheable { get; set; } = false;
        public List<GraphicElement> GraphicElements { get; set; } = new List<GraphicElement>();
        public Dictionary<string, RoleAccess> SecurityRoles { get; set; } = new Dictionary<string, RoleAccess>();

        public Popup()
        {
            ObjectType = "Popup";
        }
    }

    /// <summary>
    /// 附加图形
    /// </summary>
    public class AddOnGraphic : HmiObject
    {
        public string Description { get; set; } = "";
        public string Vendor { get; set; } = "";
        public int Major { get; set; } = 1;
        public int Minor { get; set; } = 0;
        public string Extended { get; set; } = "";
        public string Note { get; set; } = "";
        public double Width { get; set; } = 75;
        public double Height { get; set; } = 78.5;
        public double TerminalWidth { get; set; } = 800;
        public double TerminalHeight { get; set; } = 480;
        public Dictionary<string, object> UserProperties { get; set; } = new Dictionary<string, object>();
        public List<GraphicElement> GraphicElements { get; set; } = new List<GraphicElement>();

        public AddOnGraphic()
        {
            ObjectType = "AddOnGraphic";
        }
    }

    /// <summary>
    /// 视图项目
    /// </summary>
    public class ViewProject : HmiObject
    {
        public string HomeScreen { get; set; } = "";
        public List<Screen> Screens { get; set; } = new List<Screen>();
        public List<Popup> Popups { get; set; } = new List<Popup>();

        public ViewProject()
        {
            ObjectType = "ViewProject";
        }
    }
}