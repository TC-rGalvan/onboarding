using DependencyInversion;


var builder = WebApplication.CreateBuilder(args);

// Add services to the container.
//Added as a singleton to mantain in-memory collection and logger consistency to the user
builder.Services.AddSingleton<IStudentRepository, StudentRepository>();
builder.Services.AddSingleton<ILogBook, Logbook>();


builder.Services.AddControllers();
// Learn more about configuring Swagger/OpenAPI at https://aka.ms/aspnetcore/swashbuckle
builder.Services.AddEndpointsApiExplorer();
builder.Services.AddSwaggerGen();

var app = builder.Build();

// Configure the HTTP request pipeline.
if (app.Environment.IsDevelopment())
{
    app.UseSwagger();
    app.UseSwaggerUI();
}

app.UseHttpsRedirection();

app.UseAuthorization();

app.MapControllers();

app.Run();

