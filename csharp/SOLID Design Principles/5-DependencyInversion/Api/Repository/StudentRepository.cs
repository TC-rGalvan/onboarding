using System.Collections.ObjectModel;
using System;
using System.Collections.Generic;
using System.Linq;

namespace DependencyInversion
{
    public class StudentRepository : IStudentRepository
    {
        private ILogBook _logbook { get; set; }

        private static ObservableCollection<Student> collection;

        public StudentRepository(ILogBook logbook)
        {
            _logbook = logbook;
            InitData();
        }

        public void InitData()
        {
            if (collection == null) 
            {
                collection = new();
                collection.Add(new Student(1, "Pepito Pérez", new List<double>() { 3, 4.5 }));
                collection.Add(new Student(2, "Mariana Lopera", new List<double>() { 4, 5 }));
                collection.Add(new Student(3, "José Molina", new List<double>() { 2, 3 }));
            }
        }

        public IEnumerable<Student> GetAll()
        {
            _logbook.Add($"returning student's list");
            return collection;
        }

        public string Add(Student student)
        {
            string message = string.Empty;
            if(collection.Any(x => x.Id == student.Id))
            {
                message = "The student Id already exists";
                return message;
            }

            collection.Add(student);
            _logbook.Add($"The Student {student.Fullname} has been added");

            message = "Student added successfully";

            return message;
        }

         public string Remove(int studentId)
        {
            string message = string.Empty;

            var student = collection.Where(x => x.Id == studentId).FirstOrDefault();
            if(student is not null) 
            { 
                collection.Remove(student);
                message =  "Student removed successfully";
                return message;
            }
            message =  "Student was not found";

            return message;
        }
    }
}