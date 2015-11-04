PROJECT = serpents

CONFIG ?= test/test.config

RELX_URL := https://github.com/erlware/relx/releases/download/v3.5.0/relx

DEPS = recon mixer lager cowboy jiffy katana lasse epocxy trails swagger
SELL_DEPS = sync
TEST_DEPS = xref_runner shotgun

dep_lasse = git https://github.com/inaka/lasse.git 1.0.1
dep_katana = git https://github.com/inaka/erlang-katana.git 0.2.13
dep_cowboy = git https://github.com/extend/cowboy.git 1.0.3
dep_jiffy = git https://github.com/davisp/jiffy.git 0.14.3
dep_mixer = git https://github.com/inaka/mixer.git 0.1.4
dep_sync = git https://github.com/inaka/sync.git 0.1
dep_shotgun = git https://github.com/inaka/shotgun.git 0.1.12
dep_swagger = git https://github.com/inaka/cowboy-swagger 0.0.1
dep_trails = git https://github.com/inaka/cowboy-trails.git 0.0.2
dep_xref_runner = git https://github.com/inaka/xref_runner.git 0.2.2
dep_recon = git https://github.com/ferd/recon.git 2.2.1
dep_epocxy = git https://github.com/duomark/epocxy.git 0.9.9

include erlang.mk

LOCAL_DEPS := xmerl tools compiler syntax_tools common_test inets ssl public_key test_server dialyzer wx
DIALYZER_DIRS := ebin/ test/
DIALYZER_OPTS := --verbose --statistics

ERLC_OPTS := +'{parse_transform, lager_transform}'
ERLC_OPTS += +warn_unused_vars +warn_export_all +warn_shadow_vars +warn_unused_import +warn_unused_function
ERLC_OPTS += +warn_bif_clash +warn_unused_record +warn_deprecated_function +warn_obsolete_guard +strict_validation
ERLC_OPTS += +warn_export_vars +warn_exported_vars +warn_missing_spec +warn_untyped_record +debug_info

TEST_ERLC_OPTS += +'{parse_transform, lager_transform}' +debug_info
CT_OPTS += -cover test/${PROJECT}.coverspec -vvv -erl_args -config ${CONFIG}

SHELL_OPTS += -name ${PROJECT}@`hostname` -config ${CONFIG} -boot start_sasl -s lager -s sync -s ${PROJECT}

quicktests: app
	@$(MAKE) --no-print-directory app-build test-dir ERLC_OPTS="$(TEST_ERLC_OPTS)"
	$(verbose) mkdir -p $(CURDIR)/logs/
	$(gen_verbose) $(CT_RUN) -suite $(addsuffix _SUITE,$(CT_SUITES)) $(CT_OPTS)

test-build-plt: ERLC_OPTS=$(TEST_ERLC_OPTS)
test-build-plt:
	@$(MAKE) --no-print-directory test-dir ERLC_OPTS="$(TEST_ERLC_OPTS)"
	$(gen_verbose) touch ebin/test

plt-all: PLT_APPS := $(ALL_TEST_DEPS_DIRS)
plt-all: test-deps test-build-plt plt

dialyze-all: app test-build-plt dialyze

erldocs:
	erldocs . -o docs
