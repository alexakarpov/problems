(ns problems.ips)

(defn classify [s]
  (let [ipv4re #"^(?:[0-9]{1,3}\.){3}[0-9]{1,3}$"
        ipv6re #"^(?:[0-9a-fA-F]{1,4}\:){7}[0-9a-fA-F]{1,4}$"]
    (cond 
      (re-matches ipv4re s) "IPV4"
      (re-matches ipv6re s) "IPV6"
      :else "Neither")))

(defn check-ips [ip_array]
  (map classify ip_array))

(check-ips ["123.23.3.567",
           "1234:2345:3456:67:abcd:bcda:aecf:00",
           "qweasd"])
