use boa::{exec, realm::Realm};
use criterion::{black_box, criterion_group, criterion_main, Criterion};

#[cfg_attr(target = "x86_64-unknown-linux-gnu", global_allocator)]
#[cfg(target = "x86_64-unknown-linux-gnu")]
static ALLOC: jemallocator::Jemalloc = jemallocator::Jemalloc;

static SRC: &str = r#"
let a = Symbol();
let b = Symbol();
let c = Symbol();
"#;

fn symbol_creation(c: &mut Criterion) {
    c.bench_function("Symbol Creation", move |b| b.iter(|| exec(black_box(SRC))));
}

fn create_realm(c: &mut Criterion) {
    c.bench_function("Create Realm", move |b| b.iter(|| Realm::create()));
}

criterion_group!(benches, create_realm, symbol_creation);
criterion_main!(benches);
