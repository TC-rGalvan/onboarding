using OpenClose;
{
    SalaryCalculator salaryCalculator = new SalaryCalculator();

    salaryCalculator.CalculateSalaryMonthly(new List<Employee>{
                                            new EmployeeFullTime("Pepito Pérez", 160),
                                            new EmployeePartTime("Manuel Lopera", 180)});


    Console.WriteLine("Press any key to finish...");
    Console.ReadKey();
}