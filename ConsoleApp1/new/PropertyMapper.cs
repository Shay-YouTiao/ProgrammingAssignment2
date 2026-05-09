using System.Collections.Generic;

namespace HmiParser
{
    /// <summary>
    /// 属性名映射器
    /// </summary>
    public static class PropertyMapper
    {
        private static readonly Dictionary<string, string> PropertyMap = new Dictionary<string, string>(System.StringComparer.OrdinalIgnoreCase)
        {
            ["x"] = "x",
            ["y"] = "y",
            ["width"] = "width",
            ["height"] = "height",
            ["angle"] = "angle",
            ["enabled"] = "enabled",
            ["visible"] = "visible",
            ["opacity"] = "opacity",
            ["access"] = "access",
            ["forceanimations"] = "forceAnimations",
            ["fillcolor"] = "fillColor",
            ["fontname"] = "fontName",
            ["fontsize"] = "fontSize",
            ["fontcolor"] = "fontColor",
            ["text"] = "text",
            ["bold"] = "bold",
            ["underline"] = "underline",
            ["textalignment"] = "textAlignment",
            ["padding"] = "padding",
            ["cornerradius"] = "cornerRadius",
            ["showdefaultbanner"] = "showDefaultBanner",
            ["updaterate"] = "updateRate",
            ["caption"] = "caption",
            ["captionvisible"] = "captionVisible",
            ["closebuttonvisible"] = "closeButtonVisible",
            ["backgroundopacity"] = "backgroundOpacity",
            ["cacheable"] = "cacheable",
            ["homescreen"] = "homeScreen",
            ["description"] = "description",
            ["vendor"] = "vendor",
            ["major"] = "major",
            ["minor"] = "minor",
            ["extended"] = "extended",
            ["note"] = "note",
            ["terminalwidth"] = "terminalWidth",
            ["terminalheight"] = "terminalHeight",
            ["value"] = "value",
            ["minvalue"] = "minValue",
            ["maxvalue"] = "maxValue",
            ["levelcolor"] = "levelColor",
            ["housingcolor"] = "housingColor",
            ["tickmarkcolor"] = "tickMarkColor",
            ["fillopacity"] = "fillOpacity",
            ["tickmarkopacity"] = "tickMarkOpacity",
            ["textopacity"] = "textOpacity",
            ["usepredefineddisabled"] = "usePredefinedDisabled",
            ["expression"] = "expression",
            ["color"] = "color",
            ["line"] = "line",
            ["cap"] = "cap",
            ["join"] = "join",
            ["samplingrate"] = "samplingRate",
            ["duration"] = "duration",
            ["durationunit"] = "durationUnit",
            ["statustag"] = "statusTag",
            ["tagname"] = "tagName",
            ["navigateto"] = "navigateTo",
            ["screenname"] = "screenName",
            ["key"] = "key",
            ["requiresfocus"] = "requiresFocus",
            ["alwaystriggerreleaseevent"] = "alwaysTriggerReleaseEvent",
            ["releaseonopenclose"] = "releaseOnOpenClose",
            ["popupname"] = "popupName",
            ["title"] = "title",
            ["message"] = "message",
            ["documentname"] = "documentName",
            ["zoom"] = "zoom",
            ["currentpage"] = "currentPage",
            ["rounding"] = "rounding",
            ["leadingzerosfill"] = "leadingZerosFill",
            ["targetscreenname"] = "targetScreenName",
            ["usescreensecurity"] = "useScreenSecurity"
        };

        public static string MapPropertyName(string key)
        {
            if (PropertyMap.TryGetValue(key, out string mapped))
            {
                return mapped;
            }
            return key;
        }
    }
}