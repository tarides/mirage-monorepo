### v3.0.0 (2021-12-09)

* Include Mirage_protocols.ETHERNET module type in ethernet directly, remove
  dependency on mirage-protocols (#8 @hannesm)
* The ethernet library is now wrapped, this means a lot of API breakage.
  The most used binding, Ethernet_wire.sizeof_ethernet is now known as
  Ethernet.Packet.sizeof_ethernet. (#8 @hannesm)

### v2.2.1 (2021-10-21)

* Remove rresult dependency (#7 @hannesm)
* Avoid deprecated Cstruct.len, use Cstruct.length (#7 @hannesm)

### v2.2.0 (2019-10-30)

* Adapt to mirage-net 3.0.0 and mirage-protocols 4.0.0 changes (#6 @hannesm)

### v2.1.0 (2019-07-15)

* Use ipaddr.4.0.0 interfaces (#5 @avsm)

### v2.0.0 (2019-02-24)

* Adjust to mirage-protocols 2.0.0 and mirage-net 2.0.0 changes
* Ethernet is now responsible for prefixing the Ethernet header
* MTU is required from the underlying network interface instead of as argument
  to connect
* Ethif/ETHIF are now Ethernet/ETHERNET

### v1.0.0 (2019-02-01)

* Minor ocamldoc improvements (@avsm).
* Initial import from mirage-tcpip (@hannesm).
  Based on source code from mirage-tcpip.3.6.0.
