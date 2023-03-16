enum Token {
    AbsType,
    And,
    Andalso,
    As,
    Case,
    DataType,
    Do,
    Else,
    End,
    Exception,
    Fn,
    Fun,
    Handle,
    If,
    In,
    Infix,
    Infixr,
    Let,
    Local,
    NonFix,
    Of,
    Op,
    Open,
    Orelse,
    Raise,
    Rec,
    Then,
    Type,
    Val,
    With,
    WithType,
    While,
    LPar,
    RPar,
    LBrace,
    RBrace,
    LCBrace,
    RCBrace,
    Comma,
    Colon,
    SemiColon,
    ThreeDots,
    UnderBar,
    VerticalBar,
    Eq,
    DArrow,
    SArrow,
    Sharp,
    EqType,
    Functor,
    Include,
    Sharing,
    Sig,
    Sinature,
    Struct,
    Structure,
    Where,
    Opaque,
}
struct FunId(String);
struct StrId(String);
struct SigId(String);
/// structure expressions
enum StrExp {
    Basic(Box<StrDec>),
    LongStride,
    TransparentConstraint(Box<StrExp>, Box<SigExp>),
    Opaque(Box<StrExp>, Box<SigExp>),
    FunctorApplication(FunId, Box<StrExp>),
    LocalDeclaration(Box<StrDec>, Box<StrExp>),
}

/// structure-level declarations
enum StrDec {
    Dec(Dec),
    Structure(StrBind),
    Local(Box<StrDec>, Box<StrDec>),
    Empty,
    Sequential(Box<StrDec>, Box<StrDec>),
}

/// structure Bindings
struct StrBind(Vec<(StrId, StrExp)>);
/// signature expressions
enum SigExp {
    Basic(Spec),
    Id(SigId),
    TypeRealisation(),
}

/// signature declarations
struct SigDec(SigBind);

/// signature Bindings
struct SigBind(Vec<(SigId, SigExp)>);
enum Spec {
    Value(ValDesc),
    Type(TypeDesc),
    EqType(TypeDesc),
    DataType(DataDesc),
    Replication(TyCon, TyCon),
    Exception(ExDesc),
    Structure(StrDesc),
    Include(Box<SigExp>),
    Sequential(Box<Spec>, Box<Spec>),
    Sharing(Box<Spec>),
}
enum ValDesc {}
enum TypeDesc {}
enum DataDesc {}
enum TyCon {}
enum ExDesc {}
enum StrDesc {}
enum AtExp<'src> {
    SCon,
    /// op +
    OpValueIdentifier(&'src str),
    /// x
    ValueIdentifier(&'src str),
    Record,
    /// # label
    RecordSelector(&'src str),
    /// ()
    ZeroTuple,
    /// (exp1,..,expn)
    NTuple(Vec<Exp<'src>>),
    /// [exp1,...,expn]
    List(Vec<Exp<'src>>),
    /// (exp1;...;expn)
    Sequence(Vec<Exp<'src>>),
    ///
    LocalDeclaration(Dec, Vec<Exp<'src>>, Box<Exp<'src>>),
}

struct ExpRow<'src>(Vec<(&'src str, Box<Exp<'src>>)>);

enum AppExp<'src> {
    AtExp(Box<AtExp<'src>>),
    ApplicationExpression(Box<AppExp<'src>>, Box<AtExp<'src>>),
}

enum Exp<'src> {
    InfixExp(Box<InfixExp<'src>>),
    Typed(Box<Exp<'src>>, Box<Ty>),
    Conjunction(Box<Exp<'src>>, Box<Exp<'src>>),
    Disjunction(Box<Exp<'src>>, Box<Exp<'src>>),
    HandleException(Box<Exp<'src>>, Box<Match<'src>>),
    Raise(Box<Exp<'src>>),
    Conditional(Box<Exp<'src>>, Box<Exp<'src>>, Box<Exp<'src>>),
    Iteration(Box<Exp<'src>>, Box<Exp<'src>>),
    CaseAnalysis(Box<Exp<'src>>, Box<Match<'src>>),
    Function(Box<Match<'src>>),
}
enum InfixExp<'src> {
    AppExp(Box<AppExp<'src>>),
    InfixExpression(Box<InfixExp<'src>>, &'src str, Box<InfixExp<'src>>),
}

struct Match<'src>(Vec<Mrule<'src>>);
struct Mrule<'src> {
    pattern: Box<Pat>,
    expression: Box<Exp<'src>>,
}
enum Dec {
    ValueDeclaration,
    FunctionDeclaration,
    TypeDeclaration,
    DataTypeDeclaration,
    DataTypeReplication,
    AbsTypeDeclaration,
    ExceptionDeclaration,
    LocalDeclaration,
    OpenDeclaration,
    SequentialDeclaration,
    InfixLDirective,
    InfixRDirective,
    NonFixDirective,
}
enum ValBind<'src> {
    Pattern(Vec<(Box<Pat>, Box<Exp<'src>>)>),
    Rec(Box<ValBind<'src>>),
}
struct FValBind<'src> {
    is_operator: bool,
    vid: &'src str,
    cases: Vec<(Vec<AtPat>, Option<Ty>, Box<Exp<'src>>)>,
    other: Option<Box<FValBind<'src>>>,
}
enum TypBind {}
enum DatBind {}
enum ConBind {}
enum ExBind {}
enum AtPat {
    UnderBar,
    Scon,

    Record,
}
enum PatRow {
    /// ...
    WildCard,
    ///
    PatternRow(Box<Pat>, Box<PatRow>),
}
enum Pat {
    AtPat(Box<AtPat>),
    ConstractedPattern(),
    Infixed(Box<Pat>, Box<Pat>),
    Typed(Box<Pat>),
    Layerd(),
}
enum Ty {
    TyVar,
    RecordTypeExpression,
    TypeConstruction,
    FunctionTypeExpression,
    Nested(Box<Ty>),
}
enum TyRow {}
