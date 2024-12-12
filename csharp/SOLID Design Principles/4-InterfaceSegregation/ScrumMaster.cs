namespace InterfaceSegregation
{
    public class ScrumMaster : IPlaneable, ICommunicable, IDesignable
    {
        public ScrumMaster()
        {
        }

        public void Plan() 
        {
             Console.WriteLine("I'm planning user stories");
        }

        public void Comunicate(string message) 
        {
            Console.WriteLine($"ScrumMaster: {message}");
        }

        public void Design() 
        {
            Console.WriteLine("I'm designing new futures");
        }
    }
}