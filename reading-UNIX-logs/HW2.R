t1=proc.time()
# main pattern
lines = suppressWarnings(readLines("/Users/kd/Downloads/STA 141B/MergedAuth.log"))

keep = !grepl("^#", lines) & !grepl("^\\s*$", lines) # excludes 3 whitespace and 5 header lines

pattern = paste0(
  "^([A-Z][a-z]{2}\\s{1,2}\\d{1,2}\\s+\\d{2}:\\d{2}:\\d{2})", # date-time
  "\\s+",
  "([A-Za-z0-9._-]+)", # logging host
  "\\s+",
  "([A-Za-z0-9\\._-]+)(\\s[A-Za-z0-9._-]+)?(\\[\\d+\\])?", # app and PID if PID exists (accounting for sudo)
  ":\\s",
  "(.+)$"
)

matched_logical = grepl(pattern, lines[keep])
matches = regexec(pattern, lines[keep])
parsed = regmatches(lines[keep], matches)

total_lines = length(lines[keep])
match_count = sum(matched_logical)
nomatch_count = sum(!matched_logical)
cat("Total lines: ", total_lines, "\n")
cat("Matched: ", match_count, "\n")
cat("Not matched: ", nomatch_count, "\n")

nonmatch_lines = lines[!matched_logical]
#head(nonmatch_lines, 50) # sshd(pam_unix)[19939] 
#tail(nonmatch_lines, 50) # sandboxd[129] ([10018])
# com.apple.xpc.launchd[1] (com.apple.xpc.launchd.domain.pid.WebContent.37963) 


# construct df
log_labels = character(length(lines)) # construct log file for each line

current_label = NA_character_
for (i in seq_along(lines)) {
  if (grepl("^#", lines[i])) {
    current_label = sub("^#\\s*", "", lines[i]) # cleans out the #
  }
  log_labels[i] = current_label
}

# extract column from parsed, NA for non-matches
pull_col = function(list, index) {
  sapply(list, function(x) if (length(x) == 0) NA_character_ else x[index])
}

# dataframe
df = data.frame(
  date_time = pull_col(parsed, 2), # regmatches has entire line as 1
  logging_host = pull_col(parsed, 3),
  app = paste0(
    pull_col(parsed, 4),
    ifelse(is.na(pull_col(parsed, 5)), "", pull_col(parsed, 5))),
  pid = gsub("\\[|\\]", "", pull_col(parsed, 6)),
  message = pull_col(parsed, 7),
  log_file = log_labels[keep],
  matched = matched_logical
)

df[df == ""] = NA # convert blank PID to NAs
head(df)


# special patterns
# case 1: app(module)[pid] e.g., sshd(pam_unix)[19939], su(pam_unix)[30999]
pattern_pam = paste0(
  "^([A-Z][a-z]{2}\\s{1,2}\\d{1,2}\\s+\\d{2}:\\d{2}:\\d{2})",
  "\\s+",
  "([A-Za-z0-9._-]+)",
  "\\s+",
  "([A-Za-z0-9._-]+\\([A-Za-z0-9._-]+\\))",  # app 
  "(\\[\\d+\\])?",                           # optional pid
  ":\\s",
  "(.+)$"
)

# case 2: app[number part of app] ([pid]) e.g., sandboxd[129] ([10018]) 
pattern_sandbox = paste0(
  "^([A-Z][a-z]{2}\\s{1,2}\\d{1,2}\\s+\\d{2}:\\d{2}:\\d{2})",
  "\\s+",
  "([A-Za-z0-9._-]+)",
  "\\s+",
  "([A-Za-z0-9._-]+)",        # app
  "(\\[\\d+\\])",             # number part of app
  "(\\s\\(\\[\\d+\\]\\))",    # actual pid 
  ":\\s",
  "(.+)$"
)

# case 3: com.apple.xpc.launchd[1] (com.apple...WebContent.37963) (app=com.apple.xpc.launchd[1]; pid=last number)
pattern_apple = paste0(
  "^([A-Z][a-z]{2}\\s{1,2}\\d{1,2}\\s+\\d{2}:\\d{2}:\\d{2})",
  "\\s+",
  "([A-Za-z0-9._-]+)",
  "\\s+",
  "([A-Za-z0-9._-]+)",                        # app
  "(\\[\\d+\\])",                             # first number (part of app)
  "\\s(\\([A-Za-z._-]+\\d+\\))",              # descriptor, last number is pid
  ":\\s",
  "(.+)$"
)

# case 4 (singular): com.apple.xpc.launchd[1] (com.apple.WebKit.Networking.A546008E-07AF-4FFC-8FF8-D8FD260359D9[33438])
pattern_apple2 = paste0(
  "^([A-Z][a-z]{2}\\s{1,2}\\d{1,2}\\s+\\d{2}:\\d{2}:\\d{2})",
  "\\s+",
  "([A-Za-z0-9._-]+)",
  "\\s+",
  "([A-Za-z0-9._-]+)",                        # app
  "(\\[\\d+\\])",                             # first number (part of app)            
  "\\s(\\([A-Za-z0-9._-]+\\[\\d+\\]\\))",     # descriptor
  ":\\s",
  "(.+)$"
)

nonmatch_lines = lines[keep][!matched_logical]

sum(grepl(pattern_pam, nonmatch_lines))
sum(grepl(pattern_sandbox, nonmatch_lines))
sum(grepl(pattern_apple, nonmatch_lines))
sum(grepl(pattern_apple2, nonmatch_lines)) # adds to 907

# caught = grepl(pattern_pam, nonmatch_lines) |
#          grepl(pattern_sandbox, nonmatch_lines) |
#          grepl(pattern_apple, nonmatch_lines)
# nonmatch_lines[!caught]

# apple2_preview = nonmatch_lines[grepl(pattern_apple2, nonmatch_lines)]
# head(apple2_preview)

# parsing special patterns for kept lines
m_pam = regexec(pattern_pam, lines[keep])
m_sandbox = regexec(pattern_sandbox, lines[keep])
m_apple = regexec(pattern_apple, lines[keep])
m_apple2 = regexec(pattern_apple2, lines[keep])

p_pam = regmatches(lines[keep], m_pam)
p_sandbox = regmatches(lines[keep], m_sandbox)
p_apple = regmatches(lines[keep], m_apple)
p_apple2 = regmatches(lines[keep], m_apple2)

p_pam[[which(grepl(pattern_pam, lines[keep]))[1]]]
p_sandbox[[which(grepl(pattern_sandbox, lines[keep]))[1]]]
p_apple[[which(grepl(pattern_apple, lines[keep]))[1]]]
p_apple2[[which(grepl(pattern_apple2, lines[keep]))[1]]]


# special patterns into df
pam_index = which(grepl(pattern_pam, lines[keep]))
sandbox_index = which(grepl(pattern_sandbox, lines[keep]))
apple_index = which(grepl(pattern_apple, lines[keep]))
apple2_index = which(grepl(pattern_apple2, lines[keep]))

# pam
df$date_time[pam_index] = pull_col(p_pam, 2)[pam_index]
df$logging_host[pam_index] = pull_col(p_pam, 3)[pam_index]
df$app[pam_index] = pull_col(p_pam, 4)[pam_index]
df$pid[pam_index] = gsub("\\[|\\]", "", pull_col(p_pam, 5)[pam_index])
df$message[pam_index] = pull_col(p_pam, 6)[pam_index]

# sandbox
df$date_time[sandbox_index] = pull_col(p_sandbox, 2)[sandbox_index]
df$logging_host[sandbox_index] = pull_col(p_sandbox, 3)[sandbox_index]
df$app[sandbox_index] = paste0(
  pull_col(p_sandbox, 4)[sandbox_index],
  pull_col(p_sandbox, 5)[sandbox_index]
)
df$pid[sandbox_index] = gsub("\\s|\\[|\\]|\\(|\\)", "", pull_col(p_sandbox, 6)[sandbox_index])
df$message[sandbox_index] = pull_col(p_sandbox, 7)[sandbox_index]

# apple
df$date_time[apple_index] = pull_col(p_apple, 2)[apple_index]
df$logging_host[apple_index] = pull_col(p_apple, 3)[apple_index]
df$app[apple_index] = paste0(
  pull_col(p_apple, 4)[apple_index],
  pull_col(p_apple, 5)[apple_index]
)
df$pid[apple_index] = gsub(".*\\.(\\d+)\\)$", "\\1", pull_col(p_apple, 6)[apple_index])
df$message[apple_index] = pull_col(p_apple, 7)[apple_index]

# apple2
df$date_time[apple2_index] = pull_col(p_apple2, 2)[apple2_index]
df$logging_host[apple2_index] = pull_col(p_apple2, 3)[apple2_index]
df$app[apple2_index] = paste0(
  pull_col(p_apple2, 4)[apple2_index],
  pull_col(p_apple2, 5)[apple2_index]
)
df$pid[apple2_index] = gsub(".*\\[(\\d+)\\]\\)$", "\\1", pull_col(p_apple2, 6)[apple2_index])
df$message[apple2_index] = pull_col(p_apple2, 7)[apple2_index]


df$matched[c(pam_index, sandbox_index, apple_index, apple2_index)] = TRUE
sum(df$matched) # 99960 matches; all matched
sum(!df$matched) # 0 no matches

test = which(df$app == "com.apple.xpc.launchd[1]")
df[test,]


# df cleanup
df$matched = NULL # remove matched column, no longer needed
head(df)


# data validation
# log files
header_index = grep("^#", lines)
header_lines = lines[header_index]
print(header_lines) # there's 5 log files: auth.log, auth2.log, loghub/Linux/Linux_2k.log, 
# loghub/Mac/Mac_2k.log, loghub/OpenSSH/SSH_2k.log

# pids
non_numeric_pid = df$pid[!is.na(df$pid) & grepl("[^0-9]", df$pid)] # exclude NAs for lines w/out PID and exclude 0-9
unique(non_numeric_pid) # returns character(0)
df[non_numeric_pid,] # returns 0 rows, so all PID should be numeric
df$pid = as.integer(df$pid) # safe to convert

# length of log files
nrow(df[which(df$log_file == "auth.log"),]) # 86839 entries for auth.log
nrow(df[which(df$log_file == "auth2.log"),]) # 7121 entries for auth2.log
nrow(df[which(df$log_file == "loghub/Linux/Linux_2k.log"),]) # 2000 entries for loghub/Linux/Linux_2k.log
nrow(df[which(df$log_file == "loghub/Mac/Mac_2k.log"),]) # 2000 entries for loghub/Mac/Mac_2k.log
nrow(df[which(df$log_file == "loghub/OpenSSH/SSH_2k.log"),]) # 2000 entries for loghub/OpenSSH/SSH_2k.log
86839+7121+6000 # total 99960, all lines

# time ranges (LOOK AT END OF R FILE FOR MORE EXPLORATION)
# overall range -> Nov 30 to Dec 10, starting with auth.log
# and ending with OpenSSH's logs (the order in the file)
# however the years are not the same between log files
# Linux files are in 2005, MAC files are in 2017

# (missing the year for additional context)
first_date = df$date_time[1]
last_date  = df$date_time[nrow(df)]
cat("First: ", first_date, "\n")
cat("Last: ", last_date,  "\n")

months = sub("^([A-Z][a-z]{2}).*", "\\1", df$date_time)
rle(months)

# log file ranges
for (log in unique(df$log_file)) {                     
  log_df = df[df$log_file == log, ]                    
  cat("\nLog file:", log, "\n")                       
  cat("First: ", log_df$date_time[1], "\n")            
  cat("Last: ", log_df$date_time[nrow(log_df)], "\n")  
}

temp_datetime = as.POSIXct(strptime(df$date_time, format="%b %e %H:%M:%S"))

for (log in unique(df$log_file)) {
  index = which(df$log_file == log)                                   
  first = temp_datetime[index[1]]                              
  last = temp_datetime[index[length(index)]]
  cat("\nLog file:", log, "\n")
  cat("First: ", format(first, "%b %d %H:%M:%S"), "\n")
  cat("Last: ", format(last,  "%b %d %H:%M:%S"), "\n")
  cat("Span: ", difftime(last, first, units = "days"), "days\n")
}

# application numbers
apps_w_numbers = unique(df$app[grepl("[0-9]", df$app)])
apps_w_numbers # some apps contain numbers: syslogd 1.4.1, BezelServices 255.10, (versions)
# com.apple.xpc.launchd[1], sandboxd[129] (have additional structure)

unique_apps = sort(unique(df$app[!is.na(df$app)]))
unique_apps # there's 104

length(unique_apps)

app_counts = sort(table(df$app), decreasing=TRUE)
head(app_counts, 20)  

# logging host across files
for (log in unique(df$log_file)) {
  log_df = df[df$log_file == log, ]
  hosts = unique(log_df$logging_host)       # logging hosts is 1 for all auth log files EXCEPT loghub/Mac (38)
  cat("\nLog file: ", log, "\n")
  cat("Unique hosts: ", paste(hosts, collapse=", "), "\n")
  cat("Count: ", length(hosts), "\n")
}

# for each unique logging host of the files, most common app(s)
for (host in unique(df$logging_host)) {
  host_df = df[df$logging_host == host, ]
  app_counts = sort(table(host_df$app), decreasing=TRUE)
  top5 = head(app_counts, 5)
  
  cat("\n", host, "\n")
  cat(rep("-", nchar(host)), "\n", sep = "")
  for (i in seq_along(top5)) {
    cat(sprintf("  %-30s %d\n", names(top5)[i], top5[i]))
  }
}

# unique logging hosts that look like ip addresses
hosts = unique(df$logging_host)
sum(grepl("^[a-z]+-\\d{1,3}-\\d{1,3}-\\d{1,3}-\\d{1,3}", hosts)) # total 37

ip_hosts = hosts[grepl("^[a-z]+-\\d{1,3}-\\d{1,3}-\\d{1,3}-\\d{1,3}", hosts)] 
ip_host_counts = sort(table(df$logging_host[df$logging_host %in% ip_hosts]), decreasing=TRUE)
top5ip = head(ip_host_counts, 5)

for (i in seq_along(ip_host_counts)) {
  cat(sprintf("  %-30s %d\n", names(ip_host_counts)[i], ip_host_counts[i]))
}


# important apps for logins
important_apps = c("CRON", "sshd", "systemd-logind", "systemd", "sudo", "su", 
                   "sshd(pam_unix)", "su(pam_unix)", "ftpd", "klogind", "login(pam_unix)", "gdm(pam_unix)")

cat("\nTotal message counts for important apps\n")
cat(rep("=", 40), "\n", sep="")
for (app in important_apps) {
  index = df$app == app
  cat(sprintf("  %-50s %d\n", app, sum(index)))
}

show_all_messages = function(app_name) {
  index = df$app == app_name
  count = sum(index)
  cat("\nApp:", app_name, "(", count, "total )\n")
  cat(rep("-", 40), "\n", sep="")
  print(df$message[index])
}

#show_all_messages("su(pam_unix)")
#show_all_messages("login(pam_unix)")



# logins
successful_logins = df[
  !is.na(df$message) & (
    # sshd: Accepted publickey
    (df$app %in% c("sshd") & grepl("Accepted", df$message)) |
      # su/sudo/CRON/systemd/sshd/sshd(pam_unix)/login(pam_unix)/su(pam_unix): session opened
      (df$app %in% c("su", "su(pam_unix)", "sudo", "CRON", "systemd", "login(pam_unix)", "sshd", "sshd(pam_unix)") &
         grepl("session opened", df$message)) |
      # ftpd: connection from <ip>
      (df$app == "ftpd" & grepl("connection from", df$message)) |
      # systemd-logind: New session
      (df$app == "systemd-logind" & grepl("New session", df$message))
  ),
]

nrow(successful_logins)
head(successful_logins[, c("app", "message")], 10)

invalid_logins = df[
  !is.na(df$message) & (
    # sshd: Invalid user
    (df$app == "sshd" & grepl("Invalid user", df$message)) |
      # sshd(pam_unix), gdm(pam_unix): authentication failure
      (df$app %in% c("sshd(pam_unix)", "gdm(pam_unix)") & grepl("authentication failure", df$message)) |
      # klogind: authentication failed
      (df$app == "klogind" & grepl("[Aa]uthentication failed", df$message)) |
      # sshd: POSSIBLE BREAK-IN ATTEMPT 
      (df$app == "sshd" & grepl("POSSIBLE BREAK-IN ATTEMPT", df$message))
  ),
]

nrow(invalid_logins)
head(invalid_logins[, c("app", "message")], 10)

# message formats per app in successful logins
for (app in unique(successful_logins$app)) {
  cat("\nApp:", app, "\n")
  cat(rep("-", 40), "\n", sep="")
  print(head(successful_logins$message[successful_logins$app == app], 3))
}

for (app in unique(invalid_logins$app)) {
  cat("\nApp:", app, "\n")
  cat(rep("-", 40), "\n", sep="")
  print(head(invalid_logins$message[invalid_logins$app == app], 3))
}

# SUCCESS SECTION
# user and ip columns
successful_logins$user = NA_character_
successful_logins$ip = NA_character_
successful_logins$issuing_user = NA_character_ # only for sudo/su/login(pam_unix)

# sshd Accepted: "Accepted publickey for ubuntu from 85.245.107.41"
index = successful_logins$app %in% c("sshd") & 
  grepl("Accepted", successful_logins$message)
m = regexec("for (\\S+) from ([0-9.]+)", successful_logins$message[index])
p = regmatches(successful_logins$message[index], m)
successful_logins$user[index] = pull_col(p, 2)
successful_logins$ip[index] = pull_col(p, 3)

# CRON, sshd, sshd(pam_unix), systemd, sudo, su, su(pam_unix), login(pam_unix): 
# "session opened for user root by ubuntu(uid=0)" or "session opened for user test by (uid=509)"
index = !is.na(successful_logins$message) & 
  grepl("session opened for user", successful_logins$message)
m = regexec("session opened for user (\\S+) by (\\S*)\\(uid", successful_logins$message[index])
p = regmatches(successful_logins$message[index], m)
successful_logins$user[index] = pull_col(p, 2)  # target user
successful_logins$issuing_user = NA_character_ # issuing user
successful_logins$issuing_user[index] = pull_col(p, 3) 

# systemd-logind: "New session 75 of user elastic_user_7."
index = successful_logins$app == "systemd-logind"
m = regexec("of user ([A-Za-z0-9._-]+)\\.", successful_logins$message[index])
p = regmatches(successful_logins$message[index], m)
successful_logins$user[index] = pull_col(p, 2)

# ftpd: "connection from <ip>"
index = successful_logins$app == "ftpd"
m = regexec("([0-9]{1,3}\\.[0-9]{1,3}\\.[0-9]{1,3}\\.[0-9]{1,3})", 
            successful_logins$message[index])
p = regmatches(successful_logins$message[index], m)
successful_logins$ip[index] = pull_col(p, 2)

# INVALID SECTION
# initialize user and ip columns
invalid_logins$user = NA_character_
invalid_logins$ip   = NA_character_

# sshd Invalid user: "Invalid user admin from 187.12.249.74"
index = invalid_logins$app == "sshd" & 
  grepl("Invalid user", invalid_logins$message)
m = regexec("Invalid user (\\S+) from ([0-9.]+)", invalid_logins$message[index])
p = regmatches(invalid_logins$message[index], m)
invalid_logins$user[index] = pull_col(p, 2)
invalid_logins$ip[index] = pull_col(p, 3)

# sshd(pam_unix) and gdm(pam_unix) authentication failure: 
# "rhost=218.188.2.4" and optionally "user=root" or "user=guest"
# rhost can be ip (4.4.4.4) or encoded hostname (220-135-151-1.hinet-ip.hinet.net)
# rhost=220-135-151-1.hinet-ip.hinet.net OR rhost=adsl-70-242-75-179.dsl.ksc2mo.swbell.net
index = invalid_logins$app %in% c("sshd(pam_unix)", "gdm(pam_unix)") & 
  grepl("authentication failure", invalid_logins$message)
m = regexec("rhost=([A-Za-z]*-?(\\d+-\\d+-\\d+-\\d+).*)(?:\\s+user=(\\S+))?", invalid_logins$message[index])
p = regmatches(invalid_logins$message[index], m)
raw_ip = pull_col(p, 2)
# clean the ip:
# case 1: already a valid ip e.g., 218.188.2.4 -> keep as is
# case 2: encoded hostname e.g., 220-135-151-1.hinet-ip.hinet.net -> extract leading numbers and convert - to .
clean_ip = ifelse(
  grepl("^[0-9.]+$", raw_ip), # if clean ip
  raw_ip, # keep it
  gsub("-", ".", sub("^[A-Za-z]*-?(\\d+-\\d+-\\d+-\\d+).*", "\\1", raw_ip))  # or extract first 4 numbers and convert - to .
)
invalid_logins$ip[index] = clean_ip
invalid_logins$user[index] = pull_col(p, 3)

# klogind: "Authentication failed from 163.27.187.39"
index = invalid_logins$app == "klogind" & 
  grepl("[Aa]uthentication failed from", invalid_logins$message)
m = regexec("from ([0-9.]+)", invalid_logins$message[index])
p = regmatches(invalid_logins$message[index], m)
invalid_logins$ip[index] = pull_col(p, 2)

# sshd: "POSSIBLE BREAK-IN: [ip in square brackets]"
# "reverse mapping checking ... [218.26.11.118] failed - POSSIBLE BREAK-IN ATTEMPT!"
index = invalid_logins$app == "sshd" & 
  grepl("POSSIBLE BREAK-IN", invalid_logins$message)
m = regexec("\\[([0-9.]+)\\]", invalid_logins$message[index])
p = regmatches(invalid_logins$message[index], m)
invalid_logins$ip[index] = pull_col(p, 2)

for (app in unique(successful_logins$app)) {
  cat("\nApp:", app, "\n")
  cat(rep("-", 40), "\n", sep="")
  print(head(successful_logins[successful_logins$app == app, c("user", "ip", "message")], 3))
}

for (app in unique(invalid_logins$app)) {
  cat("\nApp:", app, "\n")
  cat(rep("-", 40), "\n", sep="")
  print(head(invalid_logins[invalid_logins$app == app, c("user", "ip", "message")], 3))
}

successful_logins[successful_logins == ""] = NA
invalid_logins[invalid_logins == ""] = NA


# logins cont.
head(successful_logins[!is.na(successful_logins$issuing_user) & 
                         successful_logins$issuing_user != "", 
                       c("app", "user", "issuing_user", "message")], 10)
head(invalid_logins)

has_issuing = successful_logins[!is.na(successful_logins$issuing_user), ]
nrow(has_issuing)

# successful logins users and ips
# just users
cat("Top 20 users in successful logins\n")
cat(rep("=", 40), "\n", sep="")
top_users = head(sort(table(successful_logins$user), decreasing=TRUE), 20)
for (i in seq_along(top_users)) {
  cat(sprintf("  %-30s %d\n", names(top_users)[i], top_users[i]))
}
# just ips
cat("\nTop 20 IPs in successful logins\n")
cat(rep("=", 40), "\n", sep="")
top_ips = head(sort(table(successful_logins$ip), decreasing=TRUE), 20)
for (i in seq_along(top_ips)) {
  cat(sprintf("  %-20s %d\n", names(top_ips)[i], top_ips[i]))
}
# both
cat("\nUnique user-IP pairs in successful logins\n")
cat(rep("=", 40), "\n", sep="")
user_ip_pairs = unique(successful_logins[c("user", "ip")])
user_ip_pairs = user_ip_pairs[order(user_ip_pairs$user), ]
for (i in seq_len(nrow(user_ip_pairs))) {
  cat(sprintf("  %-30s %s\n", user_ip_pairs$user[i], user_ip_pairs$ip[i]))
}

# invalid logins exploration
cat("Top 20 IP addresses in invalid logins\n")
cat(rep("=", 40), "\n", sep="")
invalid_ip_counts = sort(table(invalid_logins$ip), decreasing=TRUE)
top20_invalid_ip = head(invalid_ip_counts, 20)
for (i in seq_along(top20_invalid_ip)) { # lots of ip addresses with repeated invalid logins!
  cat(sprintf("  %-20s %d\n", names(top20_invalid_ip)[i], top20_invalid_ip[i])) 
}

# ips that appear in both invalid and successful logins -> only 1; 2 invalid attempts and 174 successes
valid_ips = unique(successful_logins$ip[!is.na(successful_logins$ip)])
invalid_ips = unique(invalid_logins$ip[!is.na(invalid_logins$ip)])

both_ips = intersect(invalid_ips, valid_ips)
cat("IPs in both invalid and successful logins\n")
cat(rep("=", 40), "\n", sep="")
cat("Count:", length(both_ips), "\n\n")

for (ip in both_ips) {
  cat("IP:", ip, "\n")
  cat("Invalid attempts:", sum(!is.na(invalid_logins$ip) & invalid_logins$ip == ip), "\n")
  cat("Successful logins:", sum(!is.na(successful_logins$ip) & successful_logins$ip == ip), "\n")
}
# are multiple ips using the same invalid login
cat("Invalid usernames attempted from multiple IPs\n") # look at each invalid username and how many IPs attempted it 
cat(rep("=", 40), "\n", sep="")
invalid_user_ip = invalid_logins[!is.na(invalid_logins$user) & # contains both user and ip
                                   !is.na(invalid_logins$ip), ]
ip_per_user = tapply(invalid_user_ip$ip, invalid_user_ip$user, 
                     function(x) length(unique(x)))
ip_per_user = sort(ip_per_user, decreasing=TRUE)
multiple_ip_users = ip_per_user[ip_per_user > 1] # only shows users w/ 2+ IPs
cat("Usernames attempted from more than one IP:", length(multiple_ip_users), "\n\n")
for (i in seq_along(multiple_ip_users)) {
  cat(sprintf("  %-30s %d IPs\n", names(multiple_ip_users)[i], multiple_ip_users[i]))
}

# are these IPs on the same network/domain
shared_netdom_count = 0
for (user in names(multiple_ip_users)) {
  ips = unique(invalid_user_ip$ip[invalid_user_ip$user == user]) # gets unique IPs per user
  ip_network_domain = sub("^(\\d+\\.\\d+\\.\\d+)\\..*", "\\1", ips)  # e.g., 218.188.2
  if (any(duplicated(ip_network_domain))) {
    shared_netdom_count = shared_netdom_count + 1
    cat("\nUser:", user, "\n")
    cat("Shared domain/network:", paste(unique(ip_network_domain[duplicated(ip_network_domain)]), collapse=", "), "\n")
  }
}
cat("Users with IPs from same network/domain:", shared_netdom_count, "\n")
cat("Users with IPs from different network/domains:", length(multiple_ip_users) - shared_netdom_count, "\n")

# authentication failure
auth_index = grepl("authentication|authenticating|authenticate", df$message, ignore.case=TRUE)
sum(auth_index, na.rm=TRUE) # 4540 lines
unique(df$app[auth_index]) # useful apps: klogind, sshd(pam_unix), sshd
# for (app in unique(df$app[auth_index])) {
#   cat("\nApp:", app, "\n")
#   cat(rep("-", 40), "\n", sep="")
#   print(head(df$message[df$app == app & auth_index], 50))
# }
auth_index = grepl("authentication|authenticating|authenticate", df$message, ignore.case = TRUE) &
  df$app %in% c("klogind", "sshd(pam_unix)", "sshd")
auth_fail_df = df[auth_index, ]
auth_fail_df$ip = NA_character_

# klogind: "Authentication failed from 163.27.187.39"
index = auth_fail_df$app == "klogind"
m = regexec("from ([0-9.]+)", auth_fail_df$message[index])
p = regmatches(auth_fail_df$message[index], m)
auth_fail_df$ip[index] = pull_col(p, 2)

# sshd(pam_unix): rhost= format
index = auth_fail_df$app == "sshd(pam_unix)"
m = regexec("rhost=([A-Za-z]*-?(\\d+-\\d+-\\d+-\\d+).*)",
            auth_fail_df$message[index])
p = regmatches(auth_fail_df$message[index], m)
raw_ip = pull_col(p, 2)
auth_fail_df$ip[index] = ifelse(
  grepl("^[0-9.]+$", raw_ip),
  raw_ip,
  gsub("-", ".", sub("^[A-Za-z]*-?(\\d+-\\d+-\\d+-\\d+).*", "\\1", raw_ip))
)

# sshd: just get ip address 
index = auth_fail_df$app == "sshd"
m = regexec("([0-9]{1,3}\\.[0-9]{1,3}\\.[0-9]{1,3}\\.[0-9]{1,3})",
            auth_fail_df$message[index])
p = regmatches(auth_fail_df$message[index], m)
auth_fail_df$ip[index] = pull_col(p, 2)

auth_fail_df$ip[auth_fail_df$ip == ""] = NA

# top IPs with most authentication failures
cat("IP addresses with most authentication failures\n")
cat(rep("=", 40), "\n", sep="")
auth_fail_counts = sort(table(auth_fail_df$ip), decreasing=TRUE)
top20_auth_fail = head(auth_fail_counts, 20)
for (i in seq_along(top20_auth_fail)) {
  cat(sprintf("  %-20s %d\n", names(top20_auth_fail)[i], top20_auth_fail[i]))
}


# sudo
# show_all_messages("sudo") # get idea of data

sudo_df = df[df$app == "sudo" & grepl("COMMAND=", df$message), ] # COMMAND holds the executable
sudo_df$user = NA_character_
sudo_df$executable = NA_character_

# user is before the first :
# command path is after COMMAND=
m = regexec("^\\s*(\\S+)\\s*:.*COMMAND=([A-Za-z0-9.-_/]+\\s*.*)$", sudo_df$message)
p = regmatches(sudo_df$message, m)
sudo_df$user = pull_col(p, 2)  # e.g., ubuntu
sudo_df$command = pull_col(p, 3)  # e.g., /usr/bin/apt-get update

exec = sub("^(\\S+)\\s*.*", "\\1", sudo_df$command)
sudo_df$executable = sub(".*/", "", exec)
sudo_table = unique(sudo_df[, c("logging_host", "user", "executable", "command")])
print(sudo_table) # respective machine and user for the executable instance

cat("Unique executables\n")
cat(rep("=", 40), "\n", sep="")
print(sort(unique(sudo_df$executable))) # the executables

na_exec_index = which(is.na(sudo_df$executable))
cat("\nMessages where executable is NA\n")
cat(rep("=", 40), "\n", sep="")
sudo_df$message[na_exec_index] # check there's no NAs and everything is matched


# datetime fix
year_pattern = "\\b(19[7-9][0-9]|200[0-9]|201[0-9]|202[0-6])\\b" # 1971-2026

for (log in unique(df$log_file)) {
  log_df = df[df$log_file == log, ]
  
  has_year = !is.na(log_df$message) & grepl(year_pattern, log_df$message)
  year_rows = log_df[has_year, ]
  
  cat("\n", rep("=", 50), "\n", sep="")
  cat("Log file:", log, "\n")
  cat("Rows with year in message:", nrow(year_rows), "\n")
  
  if (nrow(year_rows) > 0) {
    years_found = regmatches(year_rows$message, gregexpr(year_pattern, year_rows$message))
    years_flat = unlist(years_found)
    
    cat("Unique years found:", paste(sort(unique(years_flat)), collapse=", "), "\n\n")
    
    sample_rows = head(year_rows, 10)
    for (i in seq_len(nrow(sample_rows))) {
      cat(sprintf("  [%s] app=%-20s msg=%s\n",
                  sample_rows$date_time[i],
                  sample_rows$app[i],
                  sample_rows$message[i]))
    }
  } else {
    cat("  (No year mentions found in messages)\n")
  }
}

# to double check
# function to check if the year in the message is a year
explore_year = function(year, log_file_name) {
  year_pattern = paste0("\\b", year, "\\b")
  log_df = df[df$log_file == log_file_name, ]
  
  has_year = !is.na(log_df$message) & grepl(year_pattern, log_df$message)
  year_rows = log_df[has_year, ]
  
  cat(rep("=", 60), "\n", sep="")
  cat("Year:", year, "| Log file:", log_file_name, "\n")
  cat("Matching rows:", nrow(year_rows), "\n")
  cat(rep("=", 60), "\n", sep="")
  
  if (nrow(year_rows) == 0) {
    cat("No messages containing", year, "in this log file.\n")
    return(invisible(NULL))
  }
  
  for (i in seq_len(nrow(year_rows))) {
    cat(sprintf("\n[%d] %s | app: %s\n", i, year_rows$date_time[i], year_rows$app[i]))
    cat("    ", year_rows$message[i], "\n")
  }
  
  return(invisible(year_rows))
}

# explore_year(2017, "loghub/Mac/Mac_2k.log")
# explore_year(2004, "loghub/Linux/Linux_2k.log")
# explore_year(2005, "loghub/Linux/Linux_2k.log")
# 2017 for MAC
# 2005 for LINUX
print(t1)
proc.time()