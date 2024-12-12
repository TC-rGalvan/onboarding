using Microsoft.AspNetCore.Mvc;

namespace DependencyInversion.Controllers;

[ApiController, Route("student")]
public class StudentController : ControllerBase
{
    public IStudentRepository _studentRepository { get; set; }

    public StudentController(IStudentRepository studentRepository)
    {
        _studentRepository = studentRepository;
    }

    [HttpGet]
    public IActionResult Get()
    {
        return Ok(_studentRepository.GetAll());
    }

    [HttpPost]
    public IActionResult Add([FromBody]Student student)
    {
        return Ok(_studentRepository.Add(student));
    }

    [HttpDelete("{studentId}")]
    public IActionResult Remove([FromRoute]int studentId)
    {
        return Ok(_studentRepository.Remove(studentId));
    }
}
