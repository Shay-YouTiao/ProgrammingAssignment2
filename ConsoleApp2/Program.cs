using System.Collections.Generic;
using System.Text.Json;

namespace HmiParser
{
    class Program
    {
        static void Main(string[] args)
        {
            string hmiContent = @"namespace ViewDesigner::AOG;

using HMICatalog;

AddOnGraphic SimplePanel {

  Description := ""Simple three-level panel"";
  Vendor := ""Rockwell Automation"";
  Major := 1;
  Minor := 0;
  Width := 200;
  Height := 150;
  TerminalWidth := 800;
  TerminalHeight := 480;

  UserProperties {
    
    Level1_Text -> Layer1.Level1TextValue {
      Description := ""First level text property"";
      Category := ""Level1"";
    }

    Level1_Color -> Layer1.Level1ColorValue {
      Description := ""First level color property"";
      Category := ""Level1"";
    }

    Level2_Opacity -> Layer1.Layer2.Level2OpacityValue {
      Description := ""Second level opacity property"";
      Category := ""Level1.Layer2"";
    }

    Level2_Visible -> Layer1.Layer2.Level2VisibleValue {
      Description := ""Second level visibility property"";
      Category := ""Level1.Layer2"";
    }

    Level3_Speed -> Layer1.Layer2.Layer3.Level3SpeedValue {
      Description := ""Third level speed property"";
      Category := ""Level1.Layer2.Layer3"";
    }

    Level3_Mode -> Layer1.Layer2.Layer3.Level3ModeValue {
      Description := ""Third level mode property"";
      Category := ""Level1.Layer2.Layer3"";
    }
  }

  Panel Layer1 {
    FillColor := ""#00000000"";
    X := 0;
    Y := 0;
    Width := 10;
    Height := 10;

    NumericalDisplay Level1TextValue {
      Value := 0;
      FontName := ""Arial Unicode MS"";
      FontSize := 5;
    }

    NumericalDisplay Level1ColorValue {
      Value := 0;
      FontName := ""Arial Unicode MS"";
      FontSize := 5;
    }

    Panel Layer2 {
      FillColor := ""#00000000"";
      X := 0;
      Y := 0;
      Width := 10;
      Height := 10;

      NumericalDisplay Level2OpacityValue {
        Value := 0;
        FontName := ""Arial Unicode MS"";
        FontSize := 5;
      }

      NumericalDisplay Level2VisibleValue {
        Value := 0;
        FontName := ""Arial Unicode MS"";
        FontSize := 5;
      }

      Panel Layer3 {
        FillColor := ""#00000000"";
        X := 0;
        Y := 0;
        Width := 10;
        Height := 10;

        NumericalDisplay Level3SpeedValue {
          Value := 0;
          FontName := ""Arial Unicode MS"";
          FontSize := 5;
        }

        NumericalDisplay Level3ModeValue {
          Value := 0;
          FontName := ""Arial Unicode MS"";
          FontSize := 5;
        }
      }
    }
  }
}";

            var result = HmiFileParser.ParseContent(hmiContent);

            
            Console.WriteLine(result);
        }
    }
}