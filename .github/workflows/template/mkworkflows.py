#!/usr/bin/env python3
__requires__ = ["Jinja2 >= 2.11", "PyYAML >= 5.3"]
from   pathlib import Path
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
    specs_file, template_file, clients_file, workflows_dir = map(Path, sys.argv[1:])
    with specs_file.open() as fp:
        specs = yaml.safe_load(fp)
    with clients_file.open() as fp:
        clients = list(yaml.safe_load(fp).keys())
    template = template_file.read_text()
    for sp in specs:
        if sp["ostype"] == "ubuntu":
            sp["clients"] = clients
        workflow = jinja_render(template, sp)
        filename = jinja_render(template_file.with_suffix("").name, sp)
        (workflows_dir / filename).write_text(workflow + "\n")

if __name__ == "__main__":
    main()
