use boa::exec;
use criterion::{black_box, criterion_group, criterion_main, Criterion};

#[cfg(all(target_arch = "x86_64", target_os = "linux", target_env = "gnu"))]
#[cfg_attr(
  all(target_arch = "x86_64", target_os = "linux", target_env = "gnu"),
  global_allocator
)]
static ALLOC: jemallocator::Jemalloc = jemallocator::Jemalloc;

static SRC: &str = r#"
let num = 12;

function fib(n) {
  if (n <= 1) return 1;
  return fib(n - 1) + fib(n - 2);
}

let res = fib(num);

res;
"#;

fn fibonacci(c: &mut Criterion) {
  c.bench_function("fibonacci (Execution)", move |b| {
    b.iter(|| exec(black_box(SRC)))
  });
}

criterion_group!(benches, fibonacci);
criterion_main!(benches);
