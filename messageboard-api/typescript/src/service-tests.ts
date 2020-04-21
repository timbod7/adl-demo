import {PublicService, Service} from './service/service';
import {NodeHttp} from './service/node-http';
import {RESOLVER} from './adl/resolver';

async function run_tests() {
  const http = new NodeHttp();
  const pservice = new PublicService(http, "http://localhost:8080", RESOLVER);

  console.log("ping");
  await pservice.ping({});

  console.log("login as bootstrap admin user");
  const jwt = await pservice.login({
    email: "admin@test.com",
    password: "xyzzy",
  });
  const adminuser = new Service(http, "http://localhost:8080", RESOLVER, jwt);

  console.log("post messages");
  await adminuser.newMessage({body: "Hello message board!"});
  await adminuser.newMessage({body: "It's quiet around here!"});

  console.log("create a non-admin user")
  await adminuser.createUser({
    email: "user@test.com",
    password: "abcde",
    isAdmin: false
  });

  console.log("login as the new user");
  const jwt2 = await pservice.login({
    email: "user@test.com",
    password: "abcde",
  });
  const user = new Service(http, "http://localhost:8080", RESOLVER, jwt2);

  console.log("post messages");
  await user.newMessage({body: "I'm unprivileged."});

  console.log("recent messages");
  const messages = await user.recentMessages({
    maxMessages: 100
  });
  console.log(messages);

  console.log("check that the non-admin user can't create new users");
  try {
    await user.createUser({
      email: "user2@test.com",
      password: "uioqw",
      isAdmin: false
    });
  } catch (e) {
    console.log("... received expected error");
  }
}

run_tests();
