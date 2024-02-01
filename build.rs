fn main() {
    println!("cargo:rustc-link-arg=-L");
    println!("cargo:rustc-link-arg=/usr/lib/R/lib");
    //println!("cargo:rustc-link-lib=/usr/lib/R/lib");
}
