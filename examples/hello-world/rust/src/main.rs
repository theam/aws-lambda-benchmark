#[macro_use]
extern crate lambda_runtime as lambda;
#[macro_use]
extern crate serde_derive;
extern crate log;
extern crate simple_logger;

use lambda::error::HandlerError;

use std::error::Error;

#[derive(Deserialize, Clone)]
struct CustomEvent {
}

#[derive(Serialize, Clone)]
struct CustomOutput {
    statusCode: i32,
    body: String,
}

fn main() -> Result<(), Box<dyn Error>> {
    simple_logger::init_with_level(log::Level::Info)?;
    lambda!(my_handler);

    Ok(())
}

fn my_handler(_e: CustomEvent, _c: lambda::Context) -> Result<CustomOutput, HandlerError> {

    Ok(CustomOutput {
        statusCode: 200,
        body: "Hello".to_string()
    })
}
