namespace SingleResponsability
{
    public class StudentRepository
    {
        private static FakeStorage<Student> storage;

        public StudentRepository()
        {
            storage = new();
            InitData();
        }

        private void InitData()
        {
            storage.Add(new Student(1, "Pepito Pérez", new List<double>() { 3, 4.5 }));
            storage.Add(new Student(2, "Mariana Lopera", new List<double>() { 4, 5 }));
            storage.Add(new Student(3, "José Molina", new List<double>() { 2, 3 }));
            storage.Add(new Student(3, "Rodolfo Gutiérrez", new List<double>() { 2, 3, 7}));
        }

        public Student Add(Student student){
            storage.Add(student);
            return student;
        }

        public Student Remove(Student student){
            storage.Remove(student);
            return student;
        }

        public IEnumerable<Student> GetAll() 
        {
            return storage.GetAll();
        }
    }
}