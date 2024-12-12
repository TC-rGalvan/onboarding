namespace Liskov
{
    /// <summary>
    /// Data related to one employee such as personal information and hours worked.
    /// </summary>
    public class EmployeeData
    {
        public string Fullname { get; set; }
        public int HoursWorked { get; set; }
        public int ExtraHours {get;set;}
    }
}