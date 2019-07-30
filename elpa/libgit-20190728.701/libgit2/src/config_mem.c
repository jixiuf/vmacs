/*
 * Copyright (C) the libgit2 contributors. All rights reserved.
 *
 * This file is part of libgit2, distributed under the GNU GPL v2 with
 * a Linking Exception. For full terms see the included COPYING file.
 */

#include "config.h"

#include "config_backend.h"
#include "config_parse.h"
#include "config_entries.h"

typedef struct {
	git_config_backend parent;
	git_config_entries *entries;
	git_buf cfg;
} config_memory_backend;

typedef struct {
	git_config_entries *entries;
	git_config_level_t level;
} config_memory_parse_data;

static int config_error_readonly(void)
{
	giterr_set(GITERR_CONFIG, "this backend is read-only");
	return -1;
}

static int read_variable_cb(
	git_config_parser *reader,
	const char *current_section,
	const char *var_name,
	const char *var_value,
	const char *line,
	size_t line_len,
	void *payload)
{
	config_memory_parse_data *parse_data = (config_memory_parse_data *) payload;
	git_buf buf = GIT_BUF_INIT;
	git_config_entry *entry;
	const char *c;
	int result;

	GIT_UNUSED(reader);
	GIT_UNUSED(line);
	GIT_UNUSED(line_len);

	if (current_section) {
		/* TODO: Once warnings land, we should likely warn
		 * here. Git appears to warn in most cases if it sees
		 * un-namespaced config options.
		 */
		git_buf_puts(&buf, current_section);
		git_buf_putc(&buf, '.');
	}

	for (c = var_name; *c; c++)
		git_buf_putc(&buf, git__tolower(*c));

	if (git_buf_oom(&buf))
		return -1;

	entry = git__calloc(1, sizeof(git_config_entry));
	GITERR_CHECK_ALLOC(entry);
	entry->name = git_buf_detach(&buf);
	entry->value = var_value ? git__strdup(var_value) : NULL;
	entry->level = parse_data->level;
	entry->include_depth = 0;

	if ((result = git_config_entries_append(parse_data->entries, entry)) < 0)
		return result;

	return result;
}

static int config_memory_open(git_config_backend *backend, git_config_level_t level, const git_repository *repo)
{
	config_memory_backend *memory_backend = (config_memory_backend *) backend;
	config_memory_parse_data parse_data;
	git_config_parser reader;

	GIT_UNUSED(repo);

	if (memory_backend->cfg.size == 0)
		return 0;

	git_parse_ctx_init(&reader.ctx, memory_backend->cfg.ptr, memory_backend->cfg.size);
	reader.file = NULL;
	parse_data.entries = memory_backend->entries;
	parse_data.level = level;

	return git_config_parse(&reader, NULL, read_variable_cb, NULL, NULL, &parse_data);
}

static int config_memory_get(git_config_backend *backend, const char *key, git_config_entry **out)
{
	config_memory_backend *memory_backend = (config_memory_backend *) backend;
	return git_config_entries_get(out, memory_backend->entries, key);
}

static int config_memory_iterator(
	git_config_iterator **iter,
	git_config_backend *backend)
{
	config_memory_backend *memory_backend = (config_memory_backend *) backend;
	git_config_entries *entries;
	int error;

	if ((error = git_config_entries_dup(&entries, memory_backend->entries)) < 0)
		goto out;

	if ((error = git_config_entries_iterator_new(iter, entries)) < 0)
		goto out;

out:
	/* Let iterator delete duplicated entries when it's done */
	git_config_entries_free(entries);
	return error;
}

static int config_memory_set(git_config_backend *backend, const char *name, const char *value)
{
	GIT_UNUSED(backend);
	GIT_UNUSED(name);
	GIT_UNUSED(value);
	return config_error_readonly();
}

static int config_memory_set_multivar(
	git_config_backend *backend, const char *name, const char *regexp, const char *value)
{
	GIT_UNUSED(backend);
	GIT_UNUSED(name);
	GIT_UNUSED(regexp);
	GIT_UNUSED(value);
	return config_error_readonly();
}

static int config_memory_delete(git_config_backend *backend, const char *name)
{
	GIT_UNUSED(backend);
	GIT_UNUSED(name);
	return config_error_readonly();
}

static int config_memory_delete_multivar(git_config_backend *backend, const char *name, const char *regexp)
{
	GIT_UNUSED(backend);
	GIT_UNUSED(name);
	GIT_UNUSED(regexp);
	return config_error_readonly();
}

static int config_memory_lock(git_config_backend *backend)
{
	GIT_UNUSED(backend);
	return config_error_readonly();
}

static int config_memory_unlock(git_config_backend *backend, int success)
{
	GIT_UNUSED(backend);
	GIT_UNUSED(success);
	return config_error_readonly();
}

static int config_memory_snapshot(git_config_backend **out, git_config_backend *backend)
{
	GIT_UNUSED(out);
	GIT_UNUSED(backend);
	giterr_set(GITERR_CONFIG, "this backend does not support snapshots");
	return -1;
}

static void config_memory_free(git_config_backend *_backend)
{
	config_memory_backend *backend = (config_memory_backend *)_backend;

	if (backend == NULL)
		return;

	git_config_entries_free(backend->entries);
	git_buf_dispose(&backend->cfg);
	git__free(backend);
}

int git_config_backend_from_string(git_config_backend **out, const char *cfg, size_t len)
{
	config_memory_backend *backend;

	backend = git__calloc(1, sizeof(config_memory_backend));
	GITERR_CHECK_ALLOC(backend);

	if (git_config_entries_new(&backend->entries) < 0) {
		git__free(backend);
		return -1;
	}

	if (git_buf_set(&backend->cfg, cfg, len) < 0) {
		git_config_entries_free(backend->entries);
		git__free(backend);
		return -1;
	}

	backend->parent.version = GIT_CONFIG_BACKEND_VERSION;
	backend->parent.readonly = 1;
	backend->parent.open = config_memory_open;
	backend->parent.get = config_memory_get;
	backend->parent.set = config_memory_set;
	backend->parent.set_multivar = config_memory_set_multivar;
	backend->parent.del = config_memory_delete;
	backend->parent.del_multivar = config_memory_delete_multivar;
	backend->parent.iterator = config_memory_iterator;
	backend->parent.lock = config_memory_lock;
	backend->parent.unlock = config_memory_unlock;
	backend->parent.snapshot = config_memory_snapshot;
	backend->parent.free = config_memory_free;

	*out = (git_config_backend *)backend;

	return 0;
}
