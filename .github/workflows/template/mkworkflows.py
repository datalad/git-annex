#!/usr/bin/env python3
__requires__ = ["Jinja2 ~= 2.11", "PyYAML ~= 5.3"]
import os.path
import sys
import jinja2
import yaml

def jinja_render(template, context):
    """
    Custom renderer, in which we first replace GitHub Actions' ``${{`` with a
    custom placeholder to avoid Jinja tying to handle it
    """
    PLACEHOLDER = "$24$7B$7B"
    if PLACEHOLDER in template:
        raise RuntimeError(
            f"Detected placeholder {PLACEHOLDER} already present in the text"
        )
    rendered = jinja2.Template(
        template.replace("${{", PLACEHOLDER),
        trim_blocks   = True,
        lstrip_blocks = True,
     ).render(context)
    return rendered.replace(PLACEHOLDER, "${{")

def main():
    specs_file, template_file, workflows_dir = sys.argv[1:]
    with open(specs_file) as fp:
        specs = yaml.safe_load(fp)
    with open(template_file) as fp:
        template = fp.read()
    for sp in specs:
        workflow = jinja_render(template, sp)
        filename = jinja_render("build-{{ostype}}.yaml", sp)
        with open(os.path.join(workflows_dir, filename), "w") as fp:
            print(workflow, file=fp)

if __name__ == "__main__":
    main()
