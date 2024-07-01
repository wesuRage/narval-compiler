pub enum Datatype {
    Integer,
    Decimal,
    String,
    Boolean,
    Undefined,
    Null,
    Function(String), //formato disso aqui eh, hmhmhmhmhmhmmhhmhmmhmhmh, "tipo;tipo;tipo+retorno"
    Object(String),
}
