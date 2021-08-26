# How to build and run scoper

1. Make sure you have `jq` and `haskell stack` installed
1. Copy the SLA log `{"container_id":"d75d62322b868e156d087823084d8207c66ae25a54eb91c19a209890dee24c1d", . . ."@timestamp":"2021-05-12T14:21:55.000Z"}` from Splunk - you'd need the raw JSON of the event
1. Run `./runscoper.sh` - it will paste the message into the pipe to `jq`, select the `log.Message` element, and then put it into scoper through stdin 
    1. Note: this script uses `pbpaste` which is a MacOS specific command
