namespace Liskov
{
    public abstract class Employee
    {
        public EmployeeData Info { get; set; }
        public abstract int HourValue {get; set;}

        public  Employee(EmployeeData info)
        {
           Info = info;
        }  

        public virtual decimal CalculateSalary (bool IsFullTime)
        {   
            HourValue = IsFullTime ? 50 : 40;
            return HourValue * (Info.HoursWorked + Info.ExtraHours);
        } 
    }
}