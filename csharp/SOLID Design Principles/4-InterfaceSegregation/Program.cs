using InterfaceSegregation;

var project = new Project();
project.Start();
project.Design();
project.Develop();
project.Test();
project.Finish();


Console.WriteLine("Press any key to finish...");
Console.ReadKey();
