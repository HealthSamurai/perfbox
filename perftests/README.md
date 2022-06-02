TERM=xterm-256color ssh -i perf-tests-instance.pem ubuntu@44.203.237.96

hint:   https://github.com/syl20bnr/spacemacs/wiki/Terminal


Initial configuration is few VMs with aidbox dev environment


```yaml
aws configure import --csv file://new_user_credentials.csv

aws ec2 describe-instances --filters Name=tag-key,Values=Name\
--query "Reservations[*].Instances[*].{Instance:InstanceType,Instance:InstanceId,AZ:Placement.AvailabilityZone,Name:Tags[?Key=='Name']|[0].Value}" \
--output table

```
