VENV = venv

workflows : $(VENV)
	$(VENV)/bin/python mkworkflows.py specs.yml 'build-{{ostype}}.yaml.j2' ..

$(VENV) : requirements.txt
	python3 -m venv --clear $@
	$@/bin/pip install -r $<

clean :
	rm -rf $(VENV)
