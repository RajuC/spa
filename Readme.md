Smart Presence Analyzer web server

This application is a stand alone web server built on Erlang/OTP programming language.It accepts the requests(tag_id and tag_reader_id info) from the HTTP clients, process and stores the data into databases.

Libraries used:

Erlang OTP (for code development)
Cowboy Webserver
ODBC
Mnesia
MYSQL


Install the Erlang from the official site before running this applciation.
http://www.erlang.org/download.html

make necessary changes in the odbc and mysql related files 
http://www.arthurbuliva.com/content/how-use-mysql-erlang.

open mysql and run the modified_ddl.sql for creating the tables

update the mnesia directory in the rel folder(sys.config and vm.args)


go the spa folder 

cd spa/

make

./_rel/spa_release/bin/spa_release console

spa_main_service:add_location().




use another terminal for sending a request manually(for testing purpose).
curl -i -d 'tag_id=12345asdf&tag_reader_id=121230000' http://localhost:8001/wifi
HTTP/1.1 200 OK
connection: keep-alive
server: Cowboy
date: Sat, 09 Jan 2016 08:50:15 GMT
content-length: 17
content-type: text/plain



Then open the browser and http://www.localhost:8001/  ## you will see a index.html with add user and delete user for tracking.


when any requests comes from the server you can see in the erlang shell

