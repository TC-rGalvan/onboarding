namespace OpenClose
{
    public abstract class Employee
    {
        public string Fullname { get; set; }
        public int HoursWorked { get; set; }
        public decimal HourValue {get; set;}
        
        public abstract void CalculateSalaryMonthly();
    }
}