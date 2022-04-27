import pysftp

private_key = r'C:\Users\deguzman\.ssh\id_rsa-aclimate'
host = "172.30.0.31"
username = "aclimate"
#local_path = r'D:\forecast_process\workdir\ETHIOPIA\output'
local_path = r'D:\forecast_process\workdir\ETHIOPIA\output\nc_files'
remote_path = '/data/aclimate_et/'

# Ignore host check
cnopts = pysftp.CnOpts()
cnopts.hostkeys = None

# change directory on remote server
with pysftp.Connection(host, username, private_key=private_key, cnopts=cnopts) as sftp:
    print('Connected')
    sftp.put_d(local_path, remote_path, preserve_mtime=True)
    