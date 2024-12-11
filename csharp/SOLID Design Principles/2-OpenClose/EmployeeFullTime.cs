namespace OpenClose
{
    public class EmployeeFullTime : Employee
    {
        public EmployeeFullTime(string fullname, int hoursWorked)
        {
            Fullname = fullname;
            HoursWorked = hoursWorked;
            HourValue = 30000M;
        }

        public override void CalculateSalaryMonthly(){
            decimal salary = HourValue * HoursWorked;
            Console.WriteLine($"FullTime - Empleado: {Fullname}, Pago: {salary:C1} ");
        }
    }
}