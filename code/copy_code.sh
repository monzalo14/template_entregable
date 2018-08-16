#!/usr/bin/env bash
#while read script; do
#scp -i $HOME/.ssh/monizamudio_ec2.pem monizamudio@ec2-52-37-161-172.us-west-2.compute.amazonaws.com:$script $HOME/Repos/template_entregable/code/$script
#done < scripts.txt

while read file; do
    short_name=$(echo ${file##*/})
    scp -i /home/monizamudio/.ssh/monizamudio_ec2.pem monizamudio@ec2-52-37-161-172.us-west-2.compute.amazonaws.com:$file /home/monizamudio/Repos/template_entregable/code/$short_name
    source remove_accents.sh $short_name
done < scripts.txt
