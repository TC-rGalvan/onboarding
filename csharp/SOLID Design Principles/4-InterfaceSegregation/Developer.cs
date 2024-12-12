namespace InterfaceSegregation
{
    public class Developer : IDevelopable
    {
        public Developer()
        {
        }

        public void Develop() 
        {
            Console.WriteLine("I'm developing the functionalities required");
        }

        public void Comunicate(string message){
            Console.WriteLine($"Developer: {message}");
        }
    }
}