using System;
using System.Collections.Generic;
using System.IO;
using System.Text.Json;
using System.Threading.Tasks;
using System.Collections;
namespace HmiParser
{
    /// <summary>
    /// HMI 文件解析器
    /// </summary>
    public class HmiFileParser
    {
        /// <summary>
        /// 解析 .hmi 文件
        /// </summary>
        /// <param name="filePath">文件路径</param>
        /// <returns>解析结果</returns>
        public static HmiObject ParseFile(string filePath)
        {
            string content = File.ReadAllText(filePath, System.Text.Encoding.UTF8);
            return ParseContent(content);
        }

        /// <summary>
        /// 异步解析 .hmi 文件
        /// </summary>
        public static async Task<HmiObject> ParseFileAsync(string filePath)
        {
            string content = await File.ReadAllTextAsync(filePath, System.Text.Encoding.UTF8);
            return ParseContent(content);
        }

        /// <summary>
        /// 解析 .hmi 内容字符串
        /// </summary>
        /// <param name="content">内容字符串</param>
        /// <returns>解析结果</returns>
        public static HmiObject ParseContent(string content)
        {
            var parser = new Parser(content);
            return parser.Parse();
        }

        /// <summary>
        /// 解析并返回 JSON 字符串
        /// </summary>
        public static string ParseToJson(string filePath)
        {
            var result = ParseFile(filePath);
            return result.ToJson();
        }

        /// <summary>
        /// 解析并保存为 JSON 文件
        /// </summary>
        public static void ParseToJsonFile(string inputPath, string outputPath)
        {
            var result = ParseFile(inputPath);
            string json = result.ToJson();
            File.WriteAllText(outputPath, json, System.Text.Encoding.UTF8);
        }

        /// <summary>
        /// 解析整个导出的项目目录
        /// </summary>
        /// <param name="projectDirectory">项目目录路径</param>
        /// <returns>项目数据字典</returns>
        public static Dictionary<string, HmiObject> ParseProjectDirectory(string projectDirectory)
        {
            var results = new Dictionary<string, HmiObject>();

            // 解析 ViewApplication.hmi
            string viewAppPath = Path.Combine(projectDirectory, "ViewApplication.hmi");
            if (File.Exists(viewAppPath))
            {
                results["ViewApplication"] = ParseFile(viewAppPath);
            }

            // 解析 Screens 目录
            string screensDir = Path.Combine(projectDirectory, "User-Defined Screens");
            if (Directory.Exists(screensDir))
            {
                ParseDirectoryRecursive(screensDir, results, "Screen");
            }

            // 解析 Popups 目录
            string popupsDir = Path.Combine(projectDirectory, "Popups");
            if (Directory.Exists(popupsDir))
            {
                ParseDirectoryRecursive(popupsDir, results, "Popup");
            }

            // 解析 Add-On Graphics 目录
            string aogDir = Path.Combine(projectDirectory, "Assets", "Add-On Graphics");
            if (Directory.Exists(aogDir))
            {
                foreach (var file in Directory.GetFiles(aogDir, "*.hmi"))
                {
                    string key = $"AddOnGraphic_{Path.GetFileNameWithoutExtension(file)}";
                    results[key] = ParseFile(file);
                }
            }

            // 解析 Banner
            string bannerPath = Path.Combine(projectDirectory, "Predefined Screens", "banner.hmi");
            if (File.Exists(bannerPath))
            {
                results["Banner"] = ParseFile(bannerPath);
            }

            // 解析 Data Logs
            string dataLogsDir = Path.Combine(projectDirectory, "Data Logs");
            if (Directory.Exists(dataLogsDir))
            {
                foreach (var file in Directory.GetFiles(dataLogsDir, "*.hmi"))
                {
                    string key = $"DataLog_{Path.GetFileNameWithoutExtension(file)}";
                    results[key] = ParseFile(file);
                }
            }

            // 解析 Navigation Menu
            string navDir = Path.Combine(projectDirectory, "Navigation Menu");
            if (Directory.Exists(navDir))
            {
                ParseDirectoryRecursive(navDir, results, "Navigation");
            }

            return results;
        }

        private static void ParseDirectoryRecursive(string directory, Dictionary<string, HmiObject> results, string prefix)
        {
            foreach (var file in Directory.GetFiles(directory, "*.hmi"))
            {
                string fileName = Path.GetFileNameWithoutExtension(file);
                string key = $"{prefix}_{fileName}";
                try
                {
                    results[key] = ParseFile(file);
                }
                catch (Exception ex)
                {
                    results[key + "_error"] = new HmiObject
                    {
                        Name = fileName,
                        ObjectType = "Error",
                        Properties = new Dictionary<string, object>
                        {
                            ["error"] = ex.Message
                        }
                    };
                }
            }

            foreach (var subDir in Directory.GetDirectories(directory))
            {
                ParseDirectoryRecursive(subDir, results, prefix);
            }
        }

        /// <summary>
        /// 批量导出为 JSON
        /// </summary>
        public static void ExportProjectToJson(string projectDirectory, string outputDirectory)
        {
            var results = ParseProjectDirectory(projectDirectory);
            
            if (!Directory.Exists(outputDirectory))
            {
                Directory.CreateDirectory(outputDirectory);
            }

            foreach (var kvp in results)
            {
                string outputPath = Path.Combine(outputDirectory, $"{kvp.Key}.json");
                File.WriteAllText(outputPath, kvp.Value.ToJson(), System.Text.Encoding.UTF8);
            }

            // 生成汇总文件
            var summary = new Dictionary<string, object>();
            foreach (var kvp in results)
            {
                summary[kvp.Key] = new
                {
                    Type = kvp.Value.ObjectType,
                    Name = kvp.Value.Name,
                    ChildrenCount = kvp.Value.Children.Count,
                    PropertiesCount = kvp.Value.Properties.Count
                };
            }

            string summaryPath = Path.Combine(outputDirectory, "_summary.json");
            var options = new JsonSerializerOptions { WriteIndented = true };
            string summaryJson = JsonSerializer.Serialize(summary, options);
            File.WriteAllText(summaryPath, summaryJson, System.Text.Encoding.UTF8);
        }
    }
}