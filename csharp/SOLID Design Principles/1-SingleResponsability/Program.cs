using SingleResponsability;

StudentRepository studentRepository = new();

StudentExporter exporter = new ();
exporter.ExportCSV(studentRepository.GetAll());

Console.WriteLine("Process completed!");

Console.WriteLine("Press any key to finish...");
Console.ReadKey();
