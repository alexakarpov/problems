(ns problems.ips)

(defn classify [s]
  (let [ipv4re #"^(?:[0-9]{1,3}\.){3}[0-9]{1,3}$"
        ipv6re #"^(?:[0-9a-fA-F]{1,4}\:){7}[0-9a-fA-F]{1,4}$"]
    (cond 
      (re-matches ipv4re s) "IPv4"
      (re-matches ipv6re s) "IPv6"
      :else "Neither")))

(defn classify2 [s]
  (let [ipv4re #"^(?:[0-9]{1,3}\.){3}[0-9]{1,3}$"
        valid-ipv6 (fn [s]
                     (try
                       (java.net.InetAddress/getByName s)
                       true
                       (catch java.net.UnknownHostException e
                         false)))]
    (cond 
      (re-matches ipv4re s) "IPv4"
      (valid-ipv6 s) "IPv6"
      :else "Neither")))

(defn check-ips [fun ip_array]
  (map fun ip_array))

(check-ips classify ["123.23.3.567",
                     "aaaa:bbbb:cccc:1111:6789:abcd:0:0",
                     "aaaa::1111:6789:abcd:0:0",
                     "21DA::0:2F3B:2AA:FF:FE28:9C5A",
                     "qweasd"])

(check-ips classify2 ["123.23.3.567",
                      "aaaa:bbbb:cccc:1111:6789:abcd:0:0",
                      "aaaa::1111:6789:abcd:0:0",
                      "21DA:D3:0:2F3B:2AA:FF:FE28:9C5A",
                      "qweasd"])
