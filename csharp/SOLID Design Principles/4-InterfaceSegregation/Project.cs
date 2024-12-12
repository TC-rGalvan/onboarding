namespace InterfaceSegregation
{
    public class Project : IProject
    {
        public ScrumMaster ScrumMaster { get; set; }
        public Developer Developer { get; set; }
        public Tester Tester { get; set; }

        public Project(){
            ScrumMaster = new();
            Developer = new();
            Tester = new();
        }

        public void Start(){
            ScrumMaster.Plan();
            ScrumMaster.Comunicate("UserStories ready");
        }

        public void Design(){
            ScrumMaster.Design();
            ScrumMaster.Comunicate("Design of new features ready");
        }

        public void Develop(){
            Developer.Develop();
            Developer.Comunicate("Development ready");
        }

        public void Test(){
            Tester.Test();
            Tester.Comunicate("Testing ready");
        }

         public void Finish(){
            ScrumMaster.Comunicate("Project finished in time.");
        }

    }
}