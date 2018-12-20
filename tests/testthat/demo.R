
source('tests/testthat/load_test_data.R')

psa_big <- make_psa_obj(costs, effectiveness, strategies)

print(psa_big)

summary(psa_big)

plot(psa_big)

ca <- ceac(wtp, psa_obj)

print(ca)

summary(ca)

plot(ca, min_prob = 0.5)

# evpi
evpi <- calc_evpi(wtp, psa_big)
plot(evpi)
