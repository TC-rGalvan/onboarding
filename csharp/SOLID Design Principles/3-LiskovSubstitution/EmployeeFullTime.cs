namespace Liskov
{
    public class EmployeeFullTime : Employee
    {
        public override int HourValue { get; set; }

        public EmployeeFullTime(EmployeeData info) : base(info)
        {
            HourValue = 50;
        }

        public override decimal CalculateSalary (bool IsFullTime =  true)
        {   
            return HourValue * (Info.HoursWorked + Info.ExtraHours);
        } 
    }
}