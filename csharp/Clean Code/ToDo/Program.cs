using System;
using System.Collections.Generic;

namespace ToDo;

	internal class Program
	{
		public static int Option {get;set;} = 0;
		public static List<string> TaskList { get; set; } = new List<string>();

		static void Main(string[] args)
		{
			do{
				HandleMenu();
			}while(Option != OptionEnum.Exit);
		}

		public static void HandleMenu(){

				Option = ShowMainMenu();
				switch (Option)
				{
					case OptionEnum.AddTask: ShowAddTask(); break;
					case OptionEnum.RemoveTask: ShowRemoveTask(); break;
					case OptionEnum.PendingTasks: ShowPendingTasks(); break;
					case OptionEnum.Exit: ExitTaskList(); break;
					default: Console.WriteLine("Please enter a valid Option."); break;
				}
		}
		public static int ShowMainMenu()
		{
			string[] options =  {"1. New task",
								"2. Remove task",
								"3. Pending tasks",
								"4. Exit"};

			Console.WriteLine("----------------------------------------");
			Console.WriteLine("Enter the Option to perform: ");
							  
			for (int index = 0; index < options.Count(); index++)
			{
				Console.WriteLine(options[index]);
			}
		   
			return GetMenuOption();
		}

		private static int GetMenuOption()
		{
			string inputOption = Console.ReadLine();
			return Convert.ToInt32(inputOption);
		}

		public static void ShowAddTask()
		{
			try
			{
				Console.WriteLine("Enter the name of the task: ");
				string task = Console.ReadLine();
				TaskList.Add(task);
				Console.WriteLine("Task registered successfully");
			}
			catch (Exception ex)
			{
				ShowErrorMessage(ex.Message);
			}
		}

		public static void ShowRemoveTask()
		{
			try
			{
				Console.WriteLine("Enter the number of the task to remove: ");
				for (int i = 0; i < TaskList.Count; i++)
				{
					// (i + 1): add one to tasks index to show a friendly-user list
					Console.WriteLine((i + 1) + ". " + TaskList[i]);
				}
				Console.WriteLine("----------------------------------------");

				string taskIndex = Console.ReadLine();

				// Removes one index to avoid outOfIndex error.
				int indexToRemove = Convert.ToInt32(taskIndex) - 1;
				if (indexToRemove > 0)
				{
					string task = TaskList[indexToRemove];
					TaskList.RemoveAt(indexToRemove);
					Console.WriteLine("Task " + task + " deleted.");
				}
			}
			catch (Exception ex)
			{
				ShowErrorMessage(ex.Message);
			}
		}

		public static void ShowPendingTasks()
		{
			if (TaskList is null || TaskList.Count == 0)
			{
				Console.WriteLine("There are no tasks to perform.");
			} 
			else
			{
				Console.WriteLine("----------------------------------------");
				for (int i = 0; i < TaskList.Count; i++)
				{
					Console.WriteLine((i + 1) + ". " + TaskList[i]);
				}
				Console.WriteLine("----------------------------------------");
			}
		}

		private static void ExitTaskList(){
			Console.WriteLine("Exiting ToDo... See you later!");
			System.Environment.Exit(0);
		}

		/// <summary>
		/// Show Error message to avoid exiting application when an exception occurs
		/// </summary>
		 /// <param name="error">Message to show</param>
		private static void ShowErrorMessage(string error){
			Console.WriteLine($"An error has ocurred: {error}. Redirecting to main menu.");
		}
	}
	
	public enum OptionEnum 
	{
		AddTask = 1,
		RemoveTask = 2,
		PendigTasks = 3,
		Exit =  4
	}
