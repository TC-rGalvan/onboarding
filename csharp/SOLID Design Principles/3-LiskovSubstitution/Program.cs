using Liskov;

CalculateSalaryMonthly(GetEmployeesList());

void CalculateSalaryMonthly(List<Employee> employees) 
{
    foreach (var item in employees)
    {
        decimal salary = item.CalculateSalary((item is EmployeeFullTime));
        Console.WriteLine($"The {item.Info.Fullname}'s salary is {salary}");
    }

    Console.WriteLine("Press any key to finish...");
    Console.ReadKey();
}

List<Employee> GetEmployeesList(){
    return new List<Employee>()
    {
        new EmployeeFullTime(new EmployeeData{
            Fullname = "Pepito Pérez",
            HoursWorked = 160,
            ExtraHours = 10
        }),

        new EmployeeContractor(new EmployeeData{
            Fullname = "Manuel Lopera",
            HoursWorked = 180,
            ExtraHours = 0
        })
    };
}
