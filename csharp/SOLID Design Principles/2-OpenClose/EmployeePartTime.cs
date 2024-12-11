namespace OpenClose
{
    public class EmployeePartTime : Employee
    {
        public EmployeePartTime(string fullname, int hoursWorked)
        {
            Fullname = fullname;
            HoursWorked = hoursWorked;
            HourValue = 20000M;
        }
        
        public override void CalculateSalaryMonthly()
        {
            decimal salary = HourValue * HoursWorked;

            if (HoursWorked > 160) 
            {
                decimal effortCompensation = 5000M;
                int extraDays = HoursWorked - 160;
                salary += effortCompensation * extraDays;
            }

            Console.WriteLine($"PartTime - Empleado: {Fullname}, Pago: {salary:C1} ");
        }
    }
}