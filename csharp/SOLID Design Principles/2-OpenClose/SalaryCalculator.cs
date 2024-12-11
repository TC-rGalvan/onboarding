namespace OpenClose
{
    public class SalaryCalculator
    {
        public void CalculateSalaryMonthly(List<Employee> employees){
            foreach (var employee in employees)
            {
                employee.CalculateSalaryMonthly();
            }
        }

    }
}