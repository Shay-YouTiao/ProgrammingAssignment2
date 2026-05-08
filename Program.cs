using System;
using System.IO;
using System.Text.Json;
using System.Text.Encodings.Web;
using System.Text.Unicode;

namespace HmiParser
{
    class Program
    {
        static void Main(string[] args)
        {
            Console.WriteLine("Studio 5000 View Designer HMI File Parser");
            Console.WriteLine("==========================================\n");

            if (args.Length == 0)
            {
                Console.WriteLine("Usage: HmiParser.exe <input_file.hmi> [output_file.json]");
                Console.WriteLine("       Or drag and drop .hmi file onto the executable.\n");
                
                // 交互模式
                Console.Write("Enter path to .hmi file: ");
                string inputPath = Console.ReadLine()?.Trim('"');
                
                if (string.IsNullOrEmpty(inputPath))
                {
                    Console.WriteLine("No file specified. Exiting...");
                    return;
                }

                Console.Write("Enter output path (leave empty for same directory): ");
                string outputPath = Console.ReadLine()?.Trim('"');

                ProcessFile(inputPath, outputPath);
            }
            else
            {
                string outputPath = args.Length > 1 ? args[1] : null;
                ProcessFile(args[0], outputPath);
            }

            Console.WriteLine("\nPress any key to exit...");
            Console.ReadKey();
        }

        static void ProcessFile(string inputPath, string outputPath)
        {
            try
            {
                if (!File.Exists(inputPath))
                {
                    Console.WriteLine($"Error: File not found - {inputPath}");
                    return;
                }

                Console.WriteLine($"Reading file: {inputPath}");
                
                // 读取HMI文件内容
                string hmiContent = File.ReadAllText(inputPath);
                Console.WriteLine($"File size: {hmiContent.Length} characters");

                // 解析HMI文件
                Console.WriteLine("\nParsing HMI file...");
                var parser = new HmiParser();
                HmiProject project = parser.ParseFile(hmiContent);

                // 转换为JSON
                Console.WriteLine("Converting to JSON...");
                var jsonOptions = new JsonSerializerOptions
                {
                    WriteIndented = true,
                    Encoder = JavaScriptEncoder.Create(UnicodeRanges.All),
                    DefaultIgnoreCondition = System.Text.Json.Serialization.JsonIgnoreCondition.WhenWritingNull
                };

                string jsonString = JsonSerializer.Serialize(project, jsonOptions);

                // 确定输出文件路径
                if (string.IsNullOrEmpty(outputPath))
                {
                    string directory = Path.GetDirectoryName(inputPath);
                    string fileName = Path.GetFileNameWithoutExtension(inputPath);
                    outputPath = Path.Combine(directory ?? ".", $"{fileName}.json");
                }

                // 写入JSON文件
                File.WriteAllText(outputPath, jsonString);
                
                Console.WriteLine($"\nSuccess! JSON output saved to: {outputPath}");
                Console.WriteLine($"Output size: {jsonString.Length} characters");

                // 显示统计信息
                DisplayStatistics(project);
            }
            catch (Exception ex)
            {
                Console.WriteLine($"\nError: {ex.Message}");
                Console.WriteLine($"Stack trace: {ex.StackTrace}");
            }
        }

        static void DisplayStatistics(HmiProject project)
        {
            Console.WriteLine("\n=== Project Statistics ===");
            
            if (project.Project != null)
            {
                var proj = project.Project;
                Console.WriteLine($"Project Name: {proj.Name ?? "N/A"}");
                Console.WriteLine($"Home Screen: {proj.HomeScreen ?? "N/A"}");
                Console.WriteLine($"Namespace: {project.Namespace ?? "N/A"}");
                Console.WriteLine($"Using: {project.Using ?? "N/A"}");
                
                Console.WriteLine($"\nScreens: {proj.Screens?.Count ?? 0}");
                Console.WriteLine($"Popups: {proj.Popups?.Count ?? 0}");
                Console.WriteLine($"Add-On Graphics: {proj.AddOnGraphics?.Count ?? 0}");
                Console.WriteLine($"Banners: {proj.Banners?.Count ?? 0}");
                Console.WriteLine($"Data Logs: {proj.DataLogs?.Count ?? 0}");
                Console.WriteLine($"Shortcuts: {proj.Shortcuts?.Count ?? 0}");
                Console.WriteLine($"Folders: {proj.Folders?.Count ?? 0}");

                // 统计图形元素总数
                int totalElements = CountElements(proj);
                Console.WriteLine($"\nTotal Graphic Elements: {totalElements}");
            }
        }

        static int CountElements(ViewProject project)
        {
            int count = 0;
            
            CountRecursive(project.Screens, ref count);
            CountRecursive(project.Popups, ref count);
            CountRecursive(project.AddOnGraphics, ref count);
            CountRecursive(project.Banners, ref count);
            CountRecursive(project.DataLogs, ref count);
            CountRecursive(project.Shortcuts, ref count);
            CountRecursive(project.Folders, ref count);

            return count;
        }

        static void CountRecursive(List<HmiObject> objects, ref int count)
        {
            if (objects == null) return;
            
            foreach (var obj in objects)
            {
                count++;
                if (obj.Children != null)
                {
                    CountRecursive(obj.Children, ref count);
                }
                if (obj.Events != null)
                {
                    CountRecursive(obj.Events, ref count);
                }
                if (obj.Behaviors != null)
                {
                    CountRecursive(obj.Behaviors, ref count);
                }
                if (obj.StateTables != null)
                {
                    CountRecursive(obj.StateTables, ref count);
                }
            }
        }
    }
}