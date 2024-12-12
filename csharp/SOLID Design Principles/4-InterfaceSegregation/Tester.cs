namespace InterfaceSegregation
{
    public class Tester : ITestable
    {
        public Tester()
        {
        }

        public void Test() 
        {
            Console.WriteLine("I'm testing the functionalities developed.");
        }

        public void Comunicate(string message){
             Console.WriteLine($"Tester: {message}");
        }
    }
}