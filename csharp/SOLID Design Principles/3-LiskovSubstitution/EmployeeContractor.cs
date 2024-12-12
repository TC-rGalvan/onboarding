namespace Liskov
{
    public class EmployeeContractor : Employee
    {
        public override int HourValue { get; set; }
        
        public EmployeeContractor(EmployeeData info ) : base(info)
        {
            HourValue = 40;
        }

        public override decimal CalculateSalary (bool IsFullTime =  false)
        {   
            return HourValue * (Info.HoursWorked + Info.ExtraHours);
        } 
    }
}