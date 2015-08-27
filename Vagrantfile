# -*- mode: ruby -*-
# vi: set ft=ruby :

Vagrant.configure(2) do |config|
  config.vm.box = "chef/centos-6.5"

  config.vbguest.auto_update = false

  config.vm.synced_folder ".", "/vagrant"

  config.vm.provider "virtualbox" do |vb|
    vb.memory = 1024
    vb.cpus = 2
  end

  config.vm.provision "shell", inline: <<-SHELL
     sudo yum -u update
     sudo yum -y groupinstall "Development Tools"
     sudo yum -y install git curl
   SHELL
end
