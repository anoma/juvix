use methods::GUEST_ELF;
use risc0_zkvm::{default_prover, ExecutorEnv};

fn main() {
    tracing_subscriber::fmt()
        .with_env_filter(tracing_subscriber::filter::EnvFilter::from_default_env())
        .init();

    let env = ExecutorEnv::builder().build().unwrap();
    let prover = default_prover();

    let receipt = prover.prove(env, GUEST_ELF).unwrap().receipt;

    let output: u32 = receipt.journal.decode().unwrap();
    println!("{}", output);
}
