#List your VMs
gcloud compute  instances list
#Create buckets SDK
gsutil mb gs://[BUCKET_NAME]
#Copy files from directory into bucket
gsutil cp [MY_FILE] gs://[BUCKET_NAME]
#List available regions
gcloud compute regions list
#Create an environment variable and choose region
INFRACLASS_REGION=[YOUR_REGION]
#Verification of region
echo $INFRACLASS_REGION
#Make a directory
mkdir infraclass
#Create a file in the directory infraclass
touch infraclass/config
#Create an environment variable which includes the PROJECT ID into the file config
echo INFRACLASS_PROJECT_ID=$INFRACLASS_PROJECT_ID >> ~/infraclass/config
#Use the source command to set the environment variables, and use the echo command to verify that the project variable was set
source infraclass/config #you have to define it every time you open cloud shell
echo $INFRACLASS_PROJECT_ID
#create a peristent state so cloud shell has pre set source when it opens
#Edit the shell profile with the following command:
nano .profile
#Add a pesistent state in cloud shell-add this to the end of the page
source infraclass/config
##SSH stop all services
sudo /opt/bitnami/ctlscript.sh stop
##SSH restart all services
sudo /opt/bitnami/ctlscript.sh restart
#check project 
gcloud config list| grep project
#set new project
gcloud config set project $New_project
#store a project in a environment variable
export New_project=project url
#SSHTest connectivity to VM's internal IP 
ping -c 3 <Enter mynet-eu-vm's internal IP here>
#SSHTest connectivity to VM's external IP 
ping -c 3 <Enter mynet-eu-vm's external IP here>
#Create custom network
gcloud compute networks create privatenet --subnet-mode=custom
#Create subnet
gcloud compute networks subnets create privatesubnet-us --
network=privatenet --region=us-central1 --range=172.16.0.0/24
#List available VPC networks
gcloud compute networks list
#List available VPC subnets sorted by VPC network
gcloud compute networks subnets list --sort-by=NETWORK
#Create vm instance
gcloud compute instances create privatenet-us-vm --zone=us-central1-c --
machine-type=f1-micro --subnet=privatesubnet-us --image-family=debian-10 
--image-project=debian-cloud --boot-disk-size=10GB --boot-disk-type=pd-
standard --boot-disk-device-name=privatenet-us-vm
#Connect to vm instance
gcloud compute ssh vm-internal --zone us-central1-c --tunnel-through-iap
#Copy an image from other gs to our bucket
gsutil cp gs://cloud-training/gcpnet/private/access.svg gs://[my_bucket]
#SSH Resynchronize the package index (update VM instance)
sudo apt-get update
#SSH See info about unused and used memory and swap space on your VM
free
#SSH create a directory that serves as the mount point for the data disk
sudo mkdir -p /home/minecraft
#SSH To format the disk
sudo mkfs.ext4 -F -E lazy_itable_init=0,\
lazy_journal_init=0,discard \
/dev/disk/by-id/google-minecraft-disk
#SSH To mount the disk
sudo mount -o discard,defaults /dev/disk/by-id/google-minecraft-disk /home/minecraft
#SSH to install the headless JRE
sudo apt-get install -y default-jre-headless
#SSH to install wget
sudo apt-get install wget
#SSH To download the current Minecraft server JAR file (1.11.2 JAR)
sudo wget https://launcher.mojang.com/v1/objects/d0d0fe2b1dc6ab4c65554cb734270872b72dadd6/server.jar
#SSH To initialize the Minecraft server
sudo java -Xmx1024M -Xms1024M -jar server.jar nogui
#SSH To navigate to the directory where the persistent disk is mounted
cd /home/minecraft
#SSH To install screen
sudo apt-get install -y screen
#SSH To start your Minecraft server in a screen virtual terminal, run the following command: (Use the -S flag to name your terminal mcs)
sudo screen -S mcs java -Xmx1024M -Xms1024M -jar server.jar nogui
#SSH open the cron table for editing
sudo crontab -e
# To get the default access list that's been assigned to setup.html
gsutil acl get gs://$BUCKET_NAME_1/setup.html  > acl.txt
cat acl.txt
#To set the access list to private and verify the results
gsutil acl set private gs://$BUCKET_NAME_1/setup.html
gsutil acl get gs://$BUCKET_NAME_1/setup.html  > acl2.txt
cat acl2.txt
# To update the access list to make the file publicly readable
gsutil acl ch -u AllUsers:R gs://$BUCKET_NAME_1/setup.html
gsutil acl get gs://$BUCKET_NAME_1/setup.html  > acl3.txt
cat acl3.txt
#  to delete the setup file
rm setup.html
# to view the current lifecycle policy
gsutil lifecycle get gs://$BUCKET_NAME_1
# create a life.json file
nano life.json
# These instructions tell Cloud Storage to delete the object after 31 days.
{
  "rule":
  [
    {
      "action": {"type": "Delete"},
      "condition": {"age": 31}
    }
  ]
}
# To set the policy
gsutil lifecycle set life.json gs://$BUCKET_NAME_1
# to view the current versioning status for the bucket
gsutil versioning get gs://$BUCKET_NAME_1
# To enable versioning
gsutil versioning set on gs://$BUCKET_NAME_1
# Check the size of the sample file
ls -al setup.html
# Copy the file to the bucket with the -v versioning option
gsutil cp -v setup.html gs://$BUCKET_NAME_1
# Open the setup.html
nano setup.html
# To list all versions of the file
gsutil ls -a gs://$BUCKET_NAME_1/setup.html
#Make a nested directory and sync with a bucket
# Make a nested directory structure so that you can examine what happens when it is recursively copied to a bucket
mkdir firstlevel
mkdir ./firstlevel/secondlevel
cp setup.html firstlevel
cp setup.html firstlevel/secondlevel
# To sync the firstlevel directory on the VM with your bucket
gsutil rsync -r ./firstlevel gs://$BUCKET_NAME_1/firstlevel
#SSH Download the Cloud SQL Proxy and make it executable
wget https://dl.google.com/cloudsql/cloud_sql_proxy.linux.amd64 -O cloud_sql_proxy && chmod +x cloud_sql_proxy
#SSH To activate the proxy connection to your Cloud SQL database and send the process to the background
./cloud_sql_proxy -instances=$SQL_CONNECTION=tcp:3306 &
#To create a local folder and get the App Engine Hello world application
mkdir appengine-hello
cd appengine-hello
gsutil cp gs://cloud-training/archinfra/gae-hello/* .
#To run the application using the local development server in Cloud Shell Ctrl+C to exit the development server
dev_appserver.py $(pwd)
#To deploy the application to App Engine
gcloud app deploy app.yaml
#To examine the main.py file
cat main.py
#To use the sed stream editor to change the import library to the nonexistent webapp22
sed -i -e 's/webapp2/webapp22/' main.py
#To re-deploy the application to App Engine
gcloud app deploy app.yaml --quiet
#To test connectivity to server-2's external IP address
ping -c 3 <Enter server-2's external IP address here>

#Connect to the database 
gcloud sql connect databasename --user=root --quiet
#Show databases 
SHOW DATABASES;
#database and tables creation script
CREATE DATABASE IF NOT EXISTS recommendation_spark;
USE recommendation_spark;
DROP TABLE IF EXISTS Recommendation;
DROP TABLE IF EXISTS Rating;
DROP TABLE IF EXISTS Accommodation;
CREATE TABLE IF NOT EXISTS Accommodation
(
  id varchar(255),
  title varchar(255),
  location varchar(255),
  price int,
  rooms int,
  rating float,
  type varchar(255),
  PRIMARY KEY (ID)
);
CREATE TABLE  IF NOT EXISTS Rating
(
  userId varchar(255),
  accoId varchar(255),
  rating int,
  PRIMARY KEY(accoId, userId),
  FOREIGN KEY (accoId)
    REFERENCES Accommodation(id)
);
CREATE TABLE  IF NOT EXISTS Recommendation
(
  userId varchar(255),
  accoId varchar(255),
  prediction float,
  PRIMARY KEY(userId, accoId),
  FOREIGN KEY (accoId)
    REFERENCES Accommodation(id)
);
SHOW DATABASES;

#Load data from Cloud Storage into Cloud SQL tables
Click Import (top menu).
Specify the following:
Source: Click Browse > [Your-Bucket-Name] > accommodation.csv
Click Select.
Format of import: CSV
Database: select recommendation_spark from the dropdown list
Table: copy and paste: Accommodation
Click Import.

#Launch Dataproc
Go to dataproc and create a cluster with the same zone as the Cloud SQL instance
Configure nodes then click create
Then run the patching bash script into your Cloud Shell
#patching bash script
echo "Authorizing Cloud Dataproc to connect with Cloud SQL"
CLUSTER=rentals
CLOUDSQL=rentals
ZONE=us-central1-c
NWORKERS=2
machines="$CLUSTER-m"
for w in `seq 0 $(($NWORKERS - 1))`; do
   machines="$machines $CLUSTER-w-$w"
done
echo "Machines to authorize: $machines in $ZONE ... finding their IP addresses"
ips=""
for machine in $machines; do
    IP_ADDRESS=$(gcloud compute instances describe $machine --zone=$ZONE --format='value(networkInterfaces.accessConfigs[].natIP)' | sed "s/\['//g" | sed "s/'\]//g" )/32
    echo "IP address of $machine is $IP_ADDRESS"
    if [ -z  $ips ]; then
       ips=$IP_ADDRESS
    else
       ips="$ips,$IP_ADDRESS"
    fi
done
echo "Authorizing [$ips] to access cloudsql=$CLOUDSQL"
gcloud sql instances patch $CLOUDSQL --authorized-networks $ips

#Run a ml model 
Copy over the model code by executing the below commands in Cloud Shell:
gsutil cp gs://cloud-training/bdml/v2.0/model/train_and_apply.py train_and_apply.py
cloudshell edit train_and_apply.py #edit your ml model script in order to connect to the SQL instance and to database that you want 
gsutil cp train_and_apply.py gs://$DEVSHELL_PROJECT_ID #then save it and copy it in the GCS bucket

#Run your ML job on dataproc
In the Dataproc console, click cluster.
Click Submit job.
For Job type, select PySpark and for Main python file, specify the location of the Python file you uploaded to your bucket. Your <bucket-name> is likely to be your Project ID, which you can find by clicking on the Project ID dropdown in the top navigation menu.













