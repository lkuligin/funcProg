package com.lkuligin.cats.cases

import org.scalatest.{MustMatchers, WordSpec}

class UptimeClientTest extends WordSpec with MustMatchers {
  "uptimeclient" in {
    val hosts = Map("host1" -> 10, "host2" -> 6)
    val client = new TestUptimeClient(hosts)
    val service = new UptimeService(client)
    service.getTotalUptime(hosts.keys.toList) mustBe hosts.values.sum
  }

}
