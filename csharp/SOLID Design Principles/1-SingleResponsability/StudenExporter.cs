using System.Text;
namespace SingleResponsability
{
    public class StudentExporter
    {
        public StudentExporter(){}

        public void ExportCSV(IEnumerable<Student> students){
            if (students.Count() <= 0)
            {
                Console.WriteLine("There's no students to export.");
                return;
            }

            string csv = String.Join(",", students.Select(x => x.ToString()).ToArray());
            System.Text.StringBuilder sb = new System.Text.StringBuilder();
            sb.AppendLine("Id;Fullname;Grades");
            foreach (var item in students)
            {
                sb.AppendLine($"{item.Id};{item.Fullname};{string.Join("|", item.Grades)}");
            }
            System.IO.File.WriteAllText(System.IO.Path.Combine(AppDomain.CurrentDomain.BaseDirectory, "Students.csv"), sb.ToString(), Encoding.Unicode);
        }
    }
}