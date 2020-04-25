import {Service} from './service/service';
import {NodeHttp} from './service/node-http';
import {RESOLVER} from './adl/resolver';
import * as API from "./adl/api";

async function run_tests() {
  const http = new NodeHttp();
  const service = new Service(http, "http://localhost:8080", RESOLVER);

  console.log("ping");
  await service.ping({});

  console.log("login as bootstrap admin user");
  const admin = await login(service, {
      email: "admin@test.com",
      password: "xyzzy",
    });

  console.log("post messages");
  await service.newMessage(admin, {body: "Hello message board!"});
  await service.newMessage(admin, {body: "It's quiet around here!"});

  console.log("create a non-admin user")
  const resp = await service.createUser(admin, {
    email: "user@test.com",
    password: "abcde",
    isAdmin: false
  });
  if (resp.kind == "success") {
    console.log("new user created with id " + resp.value);
  }

  console.log("login as the new user");
  const user = await login(service, {
    email: "user@test.com",
    password: "abcde",
  });

  console.log("post messages");
  await service.newMessage(user, {body: "I'm unprivileged."});

  console.log("recent messages");
  const messages = await service.recentMessages(user, {
    maxMessages: 100
  });
  console.log(messages);

  console.log("check that the non-admin user can't create new users");
  try {
    await service.createUser(user, {
      email: "user2@test.com",
      password: "uioqw",
      isAdmin: false
    });
  } catch (e) {
    console.log("... received expected error");
  }
}


async function login(service: Service, req:API.LoginReq): Promise<string>  {
  const resp = await service.login(req);
  if (resp.kind == "success") {
    return resp.value;
  } else {
    throw new Error("login failed");
  }
}
run_tests();


