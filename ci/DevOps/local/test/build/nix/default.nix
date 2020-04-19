{ compiler ? "ghc865" }:

let
  config = {
    packageOverrides = pkgs: rec {
      haskell = pkgs.haskell // {
        packages = pkgs.haskell.packages // {
          "${compiler}" = pkgs.haskell.packages."${compiler}".override {
            overrides = haskellPackagesNew: haskellPackagesOld: rec {

              /*primitive-unlifted =
                haskellPackagesNew.callPackage ./primitive-unlifted.nix { };*/
              distributed-process-extras =
                haskellPackagesNew.callPackage ./distributed-process-extras.nix { };
              distributed-process-client-server =
                haskellPackagesNew.callPackage ./distributed-process-client-server.nix { };
              distributed-process-async =
                haskellPackagesNew.callPackage ./distributed-process-async.nix { };
              rank1dynamic =
                haskellPackagesNew.callPackage ./rank1dynamic.nix { };
              distributed-static =
                haskellPackagesNew.callPackage ./distributed-static.nix { };
              distributed-process =
                haskellPackagesNew.callPackage ./distributed-process.nix { };
              stm-hamt =
                haskellPackagesNew.callPackage ./stm-hamt.nix { };
              stm-containers =
                haskellPackagesNew.callPackage ./stm-containers.nix { };
              network-transport-tcp =
                haskellPackagesNew.callPackage ./network-transport-tcp.nix { };
              data-interval =
                haskellPackagesNew.callPackage ./data-interval.nix { };
              project-m36 =
                haskellPackagesNew.callPackage ./project-m36.nix { };
            };
          };
        };
      };
    };
  };

  pkgs = if builtins.pathExists ./nixpkgs then
           import (builtins.fetchGit (import ./nixpkgs)) { inherit config; }
         else
           import <nixpkgs> { inherit config; };

  inherit (pkgs) dockerTools stdenv buildEnv writeText;

  project-m36 = pkgs.haskell.packages.${compiler}.project-m36;

  static-project-m36 = pkgs.haskell.lib.justStaticExecutables pkgs.haskell.packages.${compiler}.project-m36;

  passwd = ''
    root:x:0:0::/root:/run/current-system/sw/bin/bash
    project-m36:x:90001:90001::/var/empty:/run/current-system/sw/bin/nologin
  '';

  group = ''
    root:x:0:
    nogroup:x:65534:
    project-m36:x:90001:project-m36
  '';

  nsswitch = ''
    hosts: files dns myhostname mymachines
  '';


  protocols = ''
ip	0	IP		# internet protocol, pseudo protocol number
#hopopt	0	HOPOPT		# hop-by-hop options for ipv6
icmp	1	ICMP		# internet control message protocol
igmp	2	IGMP		# internet group management protocol
ggp	3	GGP		# gateway-gateway protocol
ipencap	4	IP-ENCAP	# IP encapsulated in IP 
st2	5	ST2		# ST2 datagram mode 
tcp	6	TCP		# transmission control protocol
cbt	7	CBT		# CBT, Tony Ballardie 
egp	8	EGP		# exterior gateway protocol
igp	9	IGP		# any private interior gateway 
bbn-rcc	10	BBN-RCC-MON	# BBN RCC Monitoring
nvp	11	NVP-II		# Network Voice Protocol
pup	12	PUP		# PARC universal packet protocol
argus	13	ARGUS		# ARGUS
emcon	14	EMCON		# EMCON
xnet	15	XNET		# Cross Net Debugger
chaos	16	CHAOS		# Chaos
udp	17	UDP		# user datagram protocol
mux	18	MUX		# Multiplexing protocol
dcn	19	DCN-MEAS	# DCN Measurement Subsystems
hmp	20	HMP		# host monitoring protocol
prm	21	PRM		# packet radio measurement protocol
xns-idp	22	XNS-IDP		# Xerox NS IDP
trunk-1	23	TRUNK-1		# Trunk-1
trunk-2	24	TRUNK-2		# Trunk-2
leaf-1	25	LEAF-1		# Leaf-1
leaf-2	26	LEAF-2		# Leaf-2
rdp	27	RDP		# "reliable datagram" protocol
irtp	28	IRTP		# Internet Reliable Transaction Protocol
iso-tp4	29	ISO-TP4		# ISO Transport Protocol Class 4
netblt	30	NETBLT		# Bulk Data Transfer Protocol
mfe-nsp	31	MFE-NSP		# MFE Network Services Protocol
merit-inp	32	MERIT-INP	# MERIT Internodal Protocol
dccp	33	DCCP		# Datagram Congestion Control Protocol
3pc	34	3PC		# Third Party Connect Protocol
idpr	35	IDPR		# Inter-Domain Policy Routing Protocol
xtp	36	XTP		# Xpress Tranfer Protocol
ddp	37	DDP		# Datagram Delivery Protocol
idpr-cmtp	38	IDPR-CMTP	# IDPR Control Message Transport Proto
tp++	39	TP++		# TP++ Transport Protocol
il	40	IL		# IL Transport Protocol
ipv6	41	IPV6		# ipv6
sdrp	42	SDRP		# Source Demand Routing Protocol
ipv6-route	43	IPV6-ROUTE	# routing header for ipv6
ipv6-frag	44	IPV6-FRAG	# fragment header for ipv6
idrp	45	IDRP		# Inter-Domain Routing Protocol
rsvp	46	RSVP		# Resource ReSerVation Protocol
gre	47	GRE		# Generic Routing Encapsulation
dsr	48	DSR		# Dynamic Source Routing Protocol
bna	49	BNA		# BNA
esp	50	ESP		# encapsulating security payload
ah	51	AH		# authentication header
i-nlsp	52	I-NLSP		# Integrated Net Layer Security TUBA
swipe	53	SWIPE		# IP with Encryption
narp	54	NARP		# NBMA Address Resolution Protocol
mobile	55	MOBILE		# IP Mobility
tlsp	56	TLSP		# Transport Layer Security Protocol
skip	57	SKIP		# SKIP
ipv6-icmp	58	IPV6-ICMP	icmp6	# ICMP for IPv6
ipv6-nonxt	59	IPV6-NONXT	# no next header for ipv6
ipv6-opts	60	IPV6-OPTS	# destination options for ipv6
#	61			# any host internal protocol
cftp	62	CFTP		# CFTP
#	63			# any local network
sat-expak	64	SAT-EXPAK	# SATNET and Backroom EXPAK
kryptolan	65	KRYPTOLAN	# Kryptolan
rvd	66	RVD		# MIT Remote Virtual Disk Protocol
ippc	67	IPPC		# Internet Pluribus Packet Core
#	68			# any distributed filesystem
sat-mon	69	SAT-MON		# SATNET Monitoring
visa	70	VISA		# VISA Protocol
ipcv	71	IPCV		# Internet Packet Core Utility
cpnx	72	CPNX		# Computer Protocol Network Executive
cphb	73	CPHB		# Computer Protocol Heart Beat
wsn	74	WSN		# Wang Span Network
pvp	75	PVP		# Packet Video Protocol
br-sat-mon	76	BR-SAT-MON	# Backroom SATNET Monitoring
sun-nd	77	SUN-ND		# SUN ND PROTOCOL-Temporary
wb-mon	78	WB-MON		# WIDEBAND Monitoring
wb-expak	79	WB-EXPAK	# WIDEBAND EXPAK
iso-ip	80	ISO-IP		# ISO Internet Protocol
vmtp	81	VMTP		# Versatile Message Transport
secure-vmtp	82	SECURE-VMTP	# SECURE-VMTP
vines	83	VINES		# VINES
ttp	84	TTP		# TTP
#iptm	84	IPTM		# Protocol Internet Protocol Traffic
nsfnet-igp	85	NSFNET-IGP	# NSFNET-IGP
dgp	86	DGP		# Dissimilar Gateway Protocol
tcf	87	TCF		# TCF
eigrp	88	EIGRP		# Enhanced Interior Routing Protocol 
ospf	89	OSPFIGP		# Open Shortest Path First IGP
sprite-rpc	90	Sprite-RPC	# Sprite RPC Protocol
larp	91	LARP		# Locus Address Resolution Protocol
mtp	92	MTP		# Multicast Transport Protocol
ax.25	93	AX.25		# AX.25 Frames
ipip	94	IPIP		# Yet Another IP encapsulation
micp	95	MICP		# Mobile Internetworking Control Pro.
scc-sp	96	SCC-SP		# Semaphore Communications Sec. Pro.
etherip	97	ETHERIP		# Ethernet-within-IP Encapsulation
encap	98	ENCAP		# Yet Another IP encapsulation
#	99			# any private encryption scheme
gmtp	100	GMTP		# GMTP
ifmp	101	IFMP		# Ipsilon Flow Management Protocol
pnni	102	PNNI		# PNNI over IP
pim	103	PIM		# Protocol Independent Multicast
aris	104	ARIS		# ARIS
scps	105	SCPS		# SCPS
qnx	106	QNX		# QNX
a/n	107	A/N		# Active Networks
ipcomp	108	IPComp		# IP Payload Compression Protocol
snp	109	SNP		# Sitara Networks Protocol
compaq-peer	110	Compaq-Peer	# Compaq Peer Protocol
ipx-in-ip	111	IPX-in-IP	# IPX in IP
carp	112	CARP	vrrp		# Common Address Redundancy Protocol
pgm	113	PGM		# PGM Reliable Transport Protocol
#	114			# any 0-hop protocol
l2tp	115	L2TP		# Layer Two Tunneling Protocol
ddx	116	DDX		# D-II Data Exchange
iatp	117	IATP		# Interactive Agent Transfer Protocol
stp	118	STP		# Schedule Transfer Protocol
srp	119	SRP		# SpectraLink Radio Protocol
uti	120	UTI		# UTI
smp	121	SMP		# Simple Message Protocol
sm	122	SM		# SM
ptp	123	PTP		# Performance Transparency Protocol
isis	124	ISIS		# ISIS over IPv4
fire	125	FIRE
crtp	126	CRTP		# Combat Radio Transport Protocol
crudp	127	CRUDP		# Combat Radio User Datagram
sscopmce	128	SSCOPMCE
iplt	129	IPLT
sps	130	SPS		# Secure Packet Shield
pipe	131	PIPE		# Private IP Encapsulation within IP
sctp	132	SCTP		# Stream Control Transmission Protocol
fc	133	FC		# Fibre Channel
rsvp-e2e-ignore	134	RSVP-E2E-IGNORE	# Aggregation of RSVP for IP reservations
mobility-header	135	Mobility-Header	# Mobility Support in IPv6
udplite	136	UDPLite		# The UDP-Lite Protocol
mpls-in-ip	137	MPLS-IN-IP	# Encapsulating MPLS in IP
manet	138	MANET		# MANET Protocols
hip	139	HIP		# Host Identity Protocol 
shim6	140	SHIM6		# Shim6 Protocol 
wesp	141	WESP		# Wrapped Encapsulating Security Payload 
rohc	142	ROHC		# Robust Header Compression 
#	138-254			# Unassigned
pfsync	240	PFSYNC		# PF Synchronization
#	253-254			# Use for experimentation and testing 
#	255			# Reserved
divert	258	DIVERT		# Divert pseudo-protocol
'';

  project-m36-conf = ''
    para1 = "$(PARA1)"
    para2 = "$(PARA2)"
  '';

  project-m36-env = stdenv.mkDerivation {
    name = "project-m36-env";
    phases = [ "installPhase" "fixupPhase" ];

    installPhase = ''
      mkdir -p $out/etc/project-m36
      echo '${project-m36-conf}' > $out/etc/project-m36/project-m36.conf
      echo '${passwd}' > $out/etc/passwd
      echo '${group}' > $out/etc/group
      echo '${protocols}' > $out/etc/ptotocols
      echo '${nsswitch}' > $out/etc/nsswitch.conf
    '';
  };

  project-m36-docker =  pkgs.dockerTools.buildImage {
  name = "project-m36";
  tag = project-m36.version;
  
  contents = [ static-project-m36
               project-m36-env ];
  config = {
    Env = [ 
    "PARA1="
    "PARA2="
    ];
    User = "project-m36";
    Cmd = [ "${static-project-m36}/bin/project-m36-server" ];
    ExposedPorts = {
      "5432/tcp" = {};
    };
    WorkingDir = "/data";
    Volumes = {
      "/data" = {};
    };
  };
};
in  {
  inherit project-m36;
  inherit static-project-m36;
  inherit project-m36-docker;
}