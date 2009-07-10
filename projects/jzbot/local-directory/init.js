jzbot.load("persistent-store.js");
// var test = jzbot.createLink("irc");
// test.publicName = "initjs-mj";
// test.connect("irc.freenode.net", 6667, "");
// test.joinChannel("#nerdsunleashed.com");
// java.lang.Thread.sleep(5000);
// test.sendMessage("#nerdsunleashed.com", "Test message 3 from the new
// marlen.");
// java.lang.Thread.sleep(5000);
// test.partChannel("#nerdsunleashed.com");
// persistence.update("create table test (value varchar(512))", null);
persistence.update("delete from test", null);
persistence.update("insert into test values (#value#)",
{
	"value" :"Hello there from the database"
});
jzbot.sendToMaster("Persistence result is "
		+ persistence.selectSingleList("select * from test", null));
