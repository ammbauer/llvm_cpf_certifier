# ll-isabelle
1. Setup minimal Ubuntu 18.04
1. `wget -O - https://apt.llvm.org/llvm-snapshot.gpg.key|sudo apt-key add -`
1. `sudo add-apt-repository "deb http://apt.llvm.org/bionic/ llvm-toolchain-bionic-9 main"`
1. `apt-get install libllvm-9-ocaml-dev libllvm9 llvm-9 llvm-9-dev llvm-9-doc llvm-9-examples llvm-9-runtime clang-9`
1. `hg clone ssh://clrepo.uibk.ac.at//mercurial/rene/llvm/`
1. export code from Isabelle here
1. `stack ghci`



# Example Run
```
stack build
cat <repo-dir>/xml/even_llvm.cpf_temp.xml | stack exec ll-isabelle-exe
```



Vagrant file:
```
Vagrant.configure("2") do |config|
  config.vm.box = "generic/ubuntu1804"
  config.vm.network "forwarded_port", guest: 22, host: 7777
  config.vm.provider "virtualbox" do |vb|
    vb.memory = "4096"
  end
end
```
1. `vagrant up`
