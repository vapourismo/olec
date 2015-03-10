#include <stdbool.h>
#include <stdio.h>
#include <clutter/clutter.h>

static gboolean on_key(ClutterActor* stage, ClutterEvent* event, ClutterActor* text) {
	ClutterModifierType mod = clutter_event_get_state(event);
	guint code = clutter_event_get_key_symbol(event);

	if ((mod & CLUTTER_MODIFIER_MASK) == CLUTTER_CONTROL_MASK &&
	    code == CLUTTER_KEY_q) {
		clutter_main_quit();
		return true;
	} else {
		return false;
	}
}

int main(int argc, char** argv) {
	if (clutter_init(&argc, &argv) != CLUTTER_INIT_SUCCESS)
		return 1;

	ClutterColor bg_color = {26, 26, 26, 255};
	ClutterColor fg_color = {213, 213, 213, 255};

	// Create root stage
	ClutterActor* stage = clutter_stage_new();

	clutter_stage_set_title(CLUTTER_STAGE(stage), "Olec");
	clutter_stage_set_user_resizable(CLUTTER_STAGE(stage), true);
	clutter_actor_set_background_color(stage, &bg_color);

	// Create layout manager for root stage
	ClutterLayoutManager* layout = clutter_box_layout_new();
	clutter_box_layout_set_orientation(CLUTTER_BOX_LAYOUT(layout), CLUTTER_ORIENTATION_VERTICAL);
	clutter_actor_set_layout_manager(stage, layout);

	// Create entity
	ClutterActor* actor = clutter_actor_new();
	clutter_actor_set_background_color(actor, &bg_color);

	clutter_actor_set_x_expand(actor, true);
	clutter_actor_set_y_expand(actor, true);

	clutter_actor_set_margin_top(actor, 5);
	clutter_actor_set_margin_bottom(actor, 5);
	clutter_actor_set_margin_left(actor, 5);
	clutter_actor_set_margin_right(actor, 5);

	// Create layout for entity
	ClutterLayoutManager* actor_layout = clutter_grid_layout_new();
	clutter_grid_layout_set_orientation(CLUTTER_GRID_LAYOUT(actor_layout), CLUTTER_ORIENTATION_VERTICAL);
	clutter_actor_set_layout_manager(actor, actor_layout);

	// Add to main state
	clutter_box_layout_pack(CLUTTER_BOX_LAYOUT(layout), actor, true, true, true,
	                        CLUTTER_BOX_ALIGNMENT_CENTER, CLUTTER_BOX_ALIGNMENT_CENTER);

	// Create text
	ClutterActor* text = clutter_text_new();
	clutter_text_set_font_name(CLUTTER_TEXT(text), "Inconsolata 10.5");
	clutter_text_set_markup(CLUTTER_TEXT(text), "abc<span color='#1A1A1A' bgcolor='#D5D5D5'>d</span>efghijklmnopqrstuvwxyzabcdefghijklmnopqrstuvwxyzabcdefghijklmnopqrstuvwxyzabcdefghijklmnopqrstuvwxyzabcdefghijklmnopqrstuvwxyzabcdefghijklmnopqrstuvwxyzabcdefghijklmnopqrstuvwxyzabcdefghijklmnopqrstuvwxyzabcdefghijklmnopqrstuvwxyzabcdefghijklmnopqrstuvwxyz");
	clutter_text_set_color(CLUTTER_TEXT(text), &fg_color);

	ClutterActor* text2 = clutter_text_new();
	clutter_text_set_font_name(CLUTTER_TEXT(text2), "Inconsolata 10.5");
	clutter_text_set_markup(CLUTTER_TEXT(text2), "abcdefghijklmnopqrstuvwxyzabcdefghijklmnopqrstuvwxyzabcdefghijklmnopqrstuvwxyzabcdefghijklmnopqrstuvwxyzabcdefghijklmnopqrstuvwxyzabcdefghijklmnopqrstuvwxyzabcdefghijklmnopqrstuvwxyzabcdefghijklmnopqrstuvwxyzabcdefghijklmnopqrstuvwxyzabcdefghijklmnopqrstuvwxyz");
	clutter_text_set_color(CLUTTER_TEXT(text2), &fg_color);

	// Add to actor
	clutter_grid_layout_attach(CLUTTER_GRID_LAYOUT(actor_layout), text, 0, 0, 1, 1);
	clutter_grid_layout_attach(CLUTTER_GRID_LAYOUT(actor_layout), text2, 0, 1, 1, 1);

	// Connect destroy signal
	g_signal_connect(stage, "destroy", G_CALLBACK(clutter_main_quit), NULL);
	g_signal_connect(stage, "key-press-event", G_CALLBACK(on_key), text);

	// Show stage
	clutter_actor_show(stage);
	clutter_main();

	return 0;
}

