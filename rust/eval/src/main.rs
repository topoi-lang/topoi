use std::collections::HashMap;

enum Term {
  Var(String),
  Lam(String, Box<Term>),
  App(Box<Term>, Box<Term>),
  Let(String, Box<Term>, Box<Term>),
}

enum Val {
  Val(String),
  App(Box<Val>, Box<Val>),
  Lam(String, Fn<Val>),
}

type Env = HashMap<(String, Option<Val>)>;

struct Context {
  pub env: HashMap<(String, Val)>,
  pub local_env: HashMap<(String, Val)>,
}

impl Context {
  fn
}
fn eval(env: Env, term: Term) -> Val {
  match term {
    Val(x) => match env.get(&x) {
      None => Val::Val(x),
      Some(val) => Val::Val(val),
    },
    App(t, u) => match (eval(env, t), eval(env, u)) {
      (Val::Lam(_, t), u) => t(u),
      (t, u) => Val::App(t, u),
    },
    Lam(x, t) => {}
  }
}
