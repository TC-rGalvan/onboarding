namespace InterfaceSegregation
{
    public interface IProject
    {
        public ScrumMaster ScrumMaster { get; set; }
        public Developer Developer { get; set; }
        public Tester Tester { get; set; }

        public void Start();
        public void Finish();
    }
}