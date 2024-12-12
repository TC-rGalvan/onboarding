namespace DependencyInversion
{
    public interface IStudentRepository
    {
        public void InitData();
        public IEnumerable<Student> GetAll();
        public string Add(Student student);
        public string Remove(int studentId);
    }
}