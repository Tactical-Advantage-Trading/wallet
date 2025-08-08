package com.ornach.nobobutton;


import android.annotation.TargetApi;
import android.content.Context;
import android.content.res.ColorStateList;
import android.content.res.TypedArray;
import android.graphics.Bitmap;
import android.graphics.Canvas;
import android.graphics.Color;
import android.graphics.Paint;
import android.graphics.Typeface;
import android.graphics.drawable.Drawable;
import android.graphics.drawable.GradientDrawable;
import android.graphics.drawable.RippleDrawable;
import android.graphics.drawable.StateListDrawable;
import android.os.Build;
import android.util.AttributeSet;
import android.view.Gravity;
import android.widget.Button;
import android.widget.ImageView;
import android.widget.LinearLayout;
import android.widget.TextView;

import androidx.annotation.ColorInt;
import androidx.annotation.DrawableRes;
import androidx.annotation.RequiresApi;

import trading.tacticaladvantage.R;

public class NoboButton extends LinearLayout {

	/**
	 * icon position
	 */
	public static final int POSITION_LEFT = 1;
	public static final int POSITION_RIGHT = 2;
	public static final int POSITION_TOP = 3;
	public static final int POSITION_BOTTOM = 4;


	public static final int GRAVITY_CENTER = 0;
	public static final int GRAVITY_LEFT = 1;
	public static final int GRAVITY_RIGHT = 2;
	public static final int GRAVITY_TOP = 3;
	public static final int GRAVITY_BOTTOM = 4;


	public static final int TEXT_STYLE_NORMAL = 0;
	public static final int TEXT_STYLE_BOLD = 1;
	public static final int TEXT_STYLE_ITALIC = 2;

	private static final String TAG = Button.class.getSimpleName();

	private Context context;

	private int borderColor = Color.TRANSPARENT;
	private int borderWidth = 0;
	private float radius = 0;

	private boolean isEnabled = true;


	private int backgroundColor = Color.parseColor("#D6D7D7");
	private int focusColor = Color.parseColor("#B0B0B0");
	private int disableColor = Color.parseColor("#D6D7D7");

	private int textColor = Color.parseColor("#1C1C1C");
	private int disabledTextColor = Color.parseColor("#A0A0A0");
	private boolean textAllCaps = false;
	private int textStyle = 0;

	private int fpadding = 10, padding = 20, paddingLeft = 20, paddingTop = 20, paddingRight = 20, paddingBottom = 20;


	private CharSequence text = "";
	private int gravity = Gravity.CENTER;


	private int drawableResource = 0;
	private Drawable drawable = null;
	private String fontIcon = "";
	private int iconPosition = POSITION_LEFT;
	private int iconColor = 0;
	private int iconSize = 37;
	private int fixedIconPadding = 30, iconPadding = 0;
	private int lGravity = 0;

	private Typeface awesomeIconTypeFace = null;
	private ImageView imageView;
	private TextView textView;

	public NoboButton(Context context) {
		super(context);
		this.context = context;

		initializeView();
	}

	public NoboButton(Context context, AttributeSet attrs) {
		super(context, attrs);
		this.context = context;

		//TypedArray attrsArray = context.obtainStyledAttributes(attrs, R.styleable.CButton, 0, 0);
		/*TypedArray attrsArray = context.obtainStyledAttributes(attrs, R.styleable.CButton, 0, 0);
		initAttributes(attrsArray);
		attrsArray.recycle();*/

		processAttributes(context, attrs);

		initializeView();
	}

	@TargetApi(Build.VERSION_CODES.HONEYCOMB)
	public NoboButton(Context context, AttributeSet attrs, int defStyleAttr) {
		super(context, attrs, defStyleAttr);
		this.context = context;

		processAttributes(context, attrs);

		initializeView();
	}

	@TargetApi(Build.VERSION_CODES.LOLLIPOP)
	public NoboButton(Context context, AttributeSet attrs, int defStyleAttr, int defStyleRes) {
		super(context, attrs, defStyleAttr, defStyleRes);
		this.context = context;

		processAttributes(context, attrs);

		initializeView();
	}

	private void initializeView() {

		if (iconPosition == POSITION_TOP || iconPosition == POSITION_BOTTOM) {
			this.setOrientation(LinearLayout.VERTICAL);
		} else {
			this.setOrientation(LinearLayout.HORIZONTAL);
		}


		if (this.getLayoutParams() == null) {
			LayoutParams containerParams = new LayoutParams(LayoutParams.WRAP_CONTENT, LayoutParams.WRAP_CONTENT);
			this.setLayoutParams(containerParams);
		}


		super.setGravity(gravity);
		super.setEnabled(isEnabled);
		this.setClickable(isEnabled);
		this.setFocusable(true);


		setupTextView();

		setupImageView();
		//if (imageView != null) this.addView(imageView);

		setupBackground();

		super.setPadding(paddingLeft, paddingTop, paddingRight, paddingBottom);

		//super.setPadding(0, 0, 0, 0);


		this.removeAllViews();

		if (iconPosition == POSITION_RIGHT || iconPosition == POSITION_BOTTOM) {
			if (textView != null) this.addView(textView);
			if (imageView != null) this.addView(imageView);
		} else {
			if (imageView != null) this.addView(imageView);
			if (textView != null) this.addView(textView);

		}


		updateGravity();

	}


	private void processAttributes(final Context context, final AttributeSet attrs) {

		initDefaultAttributes17(attrs);

		TypedArray attrsArray = context.obtainStyledAttributes(attrs, R.styleable.NoboButton, 0, 0);
		initAttributes(attrsArray);
		attrsArray.recycle();
	}

	private void initDefaultAttributes(AttributeSet attrs) {
		int[] defAttr = new int[]{
			  android.R.attr.gravity,
			  android.R.attr.padding,
			  android.R.attr.paddingLeft,
			  android.R.attr.paddingTop,
			  android.R.attr.paddingRight,
			  android.R.attr.paddingBottom
		};

		TypedArray defAttrsArray = context.obtainStyledAttributes(attrs, defAttr);
		//gravity = defAttrsArray.getInt(0, gravity);
		padding = defAttrsArray.getDimensionPixelSize(1, padding);

		// initialize padding to all
		if (padding != 0) {
			paddingLeft = paddingTop = paddingRight = paddingBottom = padding;
		}

		paddingLeft = defAttrsArray.getDimensionPixelSize(2, paddingLeft);
		paddingTop = defAttrsArray.getDimensionPixelSize(3, paddingTop);
		paddingRight = defAttrsArray.getDimensionPixelSize(4, paddingRight);
		paddingBottom = defAttrsArray.getDimensionPixelSize(5, paddingBottom);
		paddingLeft = defAttrsArray.getDimensionPixelSize(6, paddingLeft);
		paddingRight = defAttrsArray.getDimensionPixelSize(7, paddingRight);


		defAttrsArray.recycle();

	}


	@RequiresApi(api = Build.VERSION_CODES.JELLY_BEAN_MR1)
	private void initDefaultAttributes17(AttributeSet attrs) {
		int[] defAttr = new int[]{
			  android.R.attr.gravity,
			  android.R.attr.padding,
			  android.R.attr.paddingLeft,
			  android.R.attr.paddingTop,
			  android.R.attr.paddingRight,
			  android.R.attr.paddingBottom,
			  android.R.attr.paddingStart,
			  android.R.attr.paddingEnd
		};

		TypedArray defAttrsArray = context.obtainStyledAttributes(attrs, defAttr);
		//gravity = defAttrsArray.getInt(0, gravity);
		padding = defAttrsArray.getDimensionPixelSize(1, padding);
		paddingLeft = paddingTop = paddingRight = paddingBottom = padding;

		paddingLeft = defAttrsArray.getDimensionPixelSize(2, paddingLeft);
		paddingTop = defAttrsArray.getDimensionPixelSize(3, paddingTop);
		paddingRight = defAttrsArray.getDimensionPixelSize(4, paddingRight);
		paddingBottom = defAttrsArray.getDimensionPixelSize(5, paddingBottom);
		paddingLeft = defAttrsArray.getDimensionPixelSize(6, paddingLeft);
		paddingRight = defAttrsArray.getDimensionPixelSize(7, paddingRight);


		defAttrsArray.recycle();

	}


	/**
	 * Initialize Attributes arrays
	 *
	 * @param attrs : Attributes array
	 */
	private void initAttributes(TypedArray attrs) {

		radius = attrs.getDimension(R.styleable.NoboButton_nb_radius, radius);

		borderColor = attrs.getColor(R.styleable.NoboButton_nb_borderColor, borderColor);
		borderWidth = (int) attrs.getDimension(R.styleable.NoboButton_nb_borderWidth, borderWidth);


		backgroundColor = attrs.getColor(R.styleable.NoboButton_nb_backgroundColor, backgroundColor);
		disableColor = attrs.getColor(R.styleable.NoboButton_nb_disableColor, disableColor);

		focusColor = attrs.getColor(R.styleable.NoboButton_nb_focusColor, focusColor);

		text = attrs.getString(R.styleable.NoboButton_nb_text);
		textColor = attrs.getColor(R.styleable.NoboButton_nb_textColor, textColor);
		disabledTextColor = attrs.getColor(R.styleable.NoboButton_nb_disabledTextColor, disabledTextColor);

		textStyle = attrs.getInt(R.styleable.NoboButton_nb_textStyle, textStyle);

		/*Log.e("TAG", "Dimension "+attrs.getDimension(R.styleable.NoboButton_textSize, textSize));
		Log.e("TAG", "DimensionPixelSize "+attrs.getDimensionPixelSize(R.styleable.NoboButton_textSize, textSize));*/

		textAllCaps = attrs.getBoolean(R.styleable.NoboButton_nb_textAllCaps, textAllCaps);


		fontIcon = attrs.getString(R.styleable.NoboButton_nb_fontIcon);
		iconSize = attrs.getDimensionPixelSize(R.styleable.NoboButton_nb_iconSize, iconSize);
		iconColor = attrs.getColor(R.styleable.NoboButton_nb_iconColor, iconColor);
		iconPosition = attrs.getInt(R.styleable.NoboButton_nb_iconPosition, iconPosition);

		drawableResource = attrs.getResourceId(R.styleable.NoboButton_nb_drawableResource, drawableResource);
		iconPadding = attrs.getDimensionPixelSize(R.styleable.NoboButton_nb_iconPadding, iconPadding);

		lGravity = attrs.getInt(R.styleable.NoboButton_nb_gravity, lGravity);
		isEnabled = attrs.getBoolean(R.styleable.NoboButton_nb_enabled, isEnabled);

		/*padding = (int) attrs.getDimension(R.styleable.NoboButton_padding,padding);
		paddingLeft = (int) attrs.getDimension(R.styleable.NoboButton_paddingLeft,paddingLeft);
		paddingTop = (int) attrs.getDimension(R.styleable.NoboButton_paddingTop,paddingTop);
		paddingRight = (int) attrs.getDimension(R.styleable.NoboButton_paddingRight,paddingRight);
		paddingBottom = (int) attrs.getDimension(R.styleable.NoboButton_paddingBottom,paddingBottom);*/
	}


	private void setupBackground() {


		// Default Drawable
		GradientDrawable defaultDrawable = new GradientDrawable();
		defaultDrawable.setCornerRadius(radius);
		defaultDrawable.setColor(backgroundColor);

		//Focus Drawable
		GradientDrawable focusDrawable = new GradientDrawable();
		focusDrawable.setCornerRadius(radius);
		focusDrawable.setColor(focusColor);

		// Disabled Drawable
		GradientDrawable disabledDrawable = new GradientDrawable();
		disabledDrawable.setCornerRadius(radius);
		disabledDrawable.setColor(disableColor);
		//disabledDrawable.setStroke(mBorderWidth, mDisabledBorderColor);


		if (borderColor != 0 && borderWidth > 0) {
			defaultDrawable.setStroke(borderWidth, borderColor);
		}


		if (Build.VERSION.SDK_INT >= Build.VERSION_CODES.LOLLIPOP) {

			setBackground(getRippleDrawable(defaultDrawable, focusDrawable, disabledDrawable));

		} else {

			StateListDrawable states = new StateListDrawable();

			// Focus/Pressed Drawable
			GradientDrawable drawable2 = new GradientDrawable();
			drawable2.setCornerRadius(radius);
			drawable2.setColor(focusColor);

			if (focusColor != 0) {
				states.addState(new int[]{android.R.attr.state_pressed}, drawable2);
				states.addState(new int[]{android.R.attr.state_focused}, drawable2);
				states.addState(new int[]{-android.R.attr.state_enabled}, disabledDrawable);
			}

			states.addState(new int[]{}, defaultDrawable);

			if (Build.VERSION.SDK_INT < Build.VERSION_CODES.JELLY_BEAN) {
				this.setBackgroundDrawable(states);
			} else {
				this.setBackground(states);
			}
		}


		/*DrawableHelper helper = new DrawableHelper.Builder()
			  .setBackgroundColor(backgroundColor)
			  .setFocusColor(focusColor)
			  .setDisabledColor(disabledColor)
			  .setBorderWidth(borderWidth)
			  .setBorderColor(borderColor)
			  .setRadius((int) radius)
			  .build();

		helper.setBackground(this, isEnabled);*/

	}


	@TargetApi(Build.VERSION_CODES.LOLLIPOP)
	private Drawable getRippleDrawable(Drawable defaultDrawable, Drawable focusDrawable, Drawable disabledDrawable) {
		if (!isEnabled()) {
			return disabledDrawable;
		} else {
			return new RippleDrawable(ColorStateList.valueOf(focusColor), defaultDrawable, focusDrawable);
		}

	}


	private void setupTextView() {
		textView = new TextView(context);
		//textView.setBackgroundColor(Color.BLUE);
		LayoutParams textViewParams = new LayoutParams(LayoutParams.WRAP_CONTENT, LayoutParams.WRAP_CONTENT);
		textView.setLayoutParams(textViewParams);

		/*if (text == null || text.isEmpty()) {
			text = "";
			return;
		}*/

		textView.setText(text);
		textView.setTextColor(isEnabled ? textColor : disabledTextColor);
		textView.setAllCaps(textAllCaps);

		// font style normal, bold, italic
		if (textStyle == 2) {
			textView.setTypeface(textView.getTypeface(), Typeface.ITALIC);
		} else if (textStyle == 1) {
			textView.setTypeface(textView.getTypeface(), Typeface.BOLD);
		} else {
			textView.setTypeface(textView.getTypeface(), Typeface.NORMAL);
		}

		//textView.setEnabled(isEnabled());

		textView.setGravity(gravity);
	}

	private void setupImageView() {
		imageView = new ImageView(context);

		/*if (drawableResource == 0 && (fontIcon == null || fontIcon.isEmpty())) {
			return;

		}*/

		//imageView = new ImageView(context);

		// change iconColor to textColor if user not defined
		if (iconColor == 0) {
			iconColor = textColor;
		}

		// add font_awesome icon to imageView
		if (fontIcon != null && !fontIcon.isEmpty()) {
			int color = isEnabled() ? iconColor : disabledTextColor;
			imageView.setImageBitmap(textToBitmap(fontIcon, iconSize, color));
		}

		// add drawable icon to imageview
		if (drawableResource != 0) {
			imageView.setImageResource(drawableResource);
		}

		if (drawable != null) {
			imageView.setImageDrawable(drawable);
		}


		updateIconPadding();

		// icon padding
		/*LayoutParams imageViewParams;
		if (iconPosition == POSITION_TOP || iconPosition == POSITION_BOTTOM) {
			imageViewParams = new LayoutParams(LayoutParams.WRAP_CONTENT, LayoutParams.WRAP_CONTENT);
		} else {
			imageViewParams = new LayoutParams(LayoutParams.WRAP_CONTENT, LayoutParams.WRAP_CONTENT);
		}*/

		/*if (textView != null && imageView != null) {
			if (iconPosition == POSITION_LEFT) {
				imageViewParams.setMargins(0, 0, getDrawablePadding(), 0);
			} else if (iconPosition == POSITION_TOP) {
				imageViewParams.setMargins(0, 0, 0, getDrawablePadding());
			} else if (iconPosition == POSITION_RIGHT) {
				imageViewParams.setMargins(getDrawablePadding(), 0, 0, 0);
			} else if (iconPosition == POSITION_BOTTOM) {
				imageViewParams.setMargins(0, getDrawablePadding(), 0, 0);
			}
		}*/


		//imageView.setEnabled(isEnabled());

		//imageView.setLayoutParams(imageViewParams);
		//imageView.setBackgroundColor(Color.RED);
	}


	private static int pxToSp(final Context context, final float px) {
		return Math.round(px / context.getResources().getDisplayMetrics().scaledDensity);
	}

	/*public static int spToPx(final Context context, final float sp) {
		return Math.round(sp * context.getResources().getDisplayMetrics().scaledDensity);
	}

	public static int pxToDp(final Context context, final float px) {
		return Math.round(px / context.getResources().getDisplayMetrics().density);
	}

	public static int dpToPx(final Context context, final float dp) {
		return Math.round(dp * context.getResources().getDisplayMetrics().density);
	}*/

	private void updateGravity() {
		if (lGravity == GRAVITY_CENTER) {
			// center
			super.setGravity(Gravity.CENTER);
		} else if (lGravity == GRAVITY_LEFT) {
			// left
			super.setGravity(Gravity.START | Gravity.CENTER_VERTICAL);
		} else if (lGravity == GRAVITY_RIGHT) {
			// right
			super.setGravity(Gravity.END | Gravity.CENTER_VERTICAL);
		} else if (lGravity == GRAVITY_TOP) {
			// top
			super.setGravity(Gravity.TOP | Gravity.CENTER_HORIZONTAL);
		} else if (lGravity == GRAVITY_BOTTOM) {
			// bottom
			super.setGravity(Gravity.BOTTOM | Gravity.CENTER_HORIZONTAL);
		}
	}

	private void updateIconPadding() {
		LayoutParams imageViewParams = new LayoutParams(LayoutParams.WRAP_CONTENT, LayoutParams.WRAP_CONTENT);

		if (fontIcon == null || fontIcon.isEmpty()|| fontIcon.length()<=0) {
			imageViewParams.setMargins(0, 0, 0, 0);
		} else if (iconPosition == POSITION_LEFT) {
			imageViewParams.setMargins(0, 0, getDrawablePadding(), 0);
		} else if (iconPosition == POSITION_TOP) {
			imageViewParams.setMargins(0, 0, 0, getDrawablePadding());
		} else if (iconPosition == POSITION_RIGHT) {
			imageViewParams.setMargins(getDrawablePadding(), 0, 0, 0);
		} else if (iconPosition == POSITION_BOTTOM) {
			imageViewParams.setMargins(0, getDrawablePadding(), 0, 0);
		}

		imageView.setLayoutParams(imageViewParams);
	}

	private int getDrawablePadding() {
		if (iconPadding != 0) {
			return iconPadding;
		}
		return fixedIconPadding;
	}


	private Bitmap getFontBitmap() {

		Paint paint = new Paint(Paint.ANTI_ALIAS_FLAG);
		paint.setColor(iconColor);

		if (awesomeIconTypeFace != null && !isInEditMode()) {
			paint.setTypeface(awesomeIconTypeFace);
			paint.setTextSize(iconSize);
		} else {
			fontIcon = "o";
			paint.setTextSize(iconSize - 15);
		}

		paint.setTextAlign(Paint.Align.LEFT);
		float baseline = -paint.ascent(); // ascent() is negative
		int width = (int) (paint.measureText(fontIcon) + 0.5f); // round
		int height = (int) (baseline + paint.descent() + 0.5f);
		Bitmap image = Bitmap.createBitmap(width, height, Bitmap.Config.ARGB_8888);
		Canvas canvas = new Canvas(image);
		canvas.drawText(fontIcon, 0, baseline, paint);
		return image;
	}

	private Bitmap textToBitmap(String text, float iconSize, int textColor) {

		/*if (awesomeIconTypeFace == null) {
			return null;
		}*/

		//float tSize = iconSize != 0 ? iconSize : spToPx(context, this.fixedTextSize);

		Paint paint = new Paint(Paint.ANTI_ALIAS_FLAG);
		paint.setColor(textColor);

		if (awesomeIconTypeFace != null && !isInEditMode()) {
			paint.setTypeface(awesomeIconTypeFace);
			paint.setTextSize(iconSize);
		} else {
			text = "O";
			paint.setTextSize((float) (iconSize / 2.5));
		}

		paint.setTextAlign(Paint.Align.LEFT);
		float baseline = -paint.ascent(); // ascent() is negative
		int width = (int) (paint.measureText(text) + 0.5f); // round
		int height = (int) (baseline + paint.descent() + 0.5f);
		Bitmap image = Bitmap.createBitmap(width, height, Bitmap.Config.ARGB_8888);
		Canvas canvas = new Canvas(image);
		canvas.drawText(text, 0, baseline, paint);
		return image;
	}

	public void setAllCaps(boolean allCaps) {
		this.textAllCaps = allCaps;
		textView.setAllCaps(allCaps);
	}

	public boolean getAllCaps() {
		return textAllCaps;
	}

	public void setText(CharSequence text) {
		this.text = text;
		if (textView != null) textView.setText(text);
		else setupTextView();
	}

	public CharSequence getText() {
		return text;
	}

	public void setTextStyle(int style) {
		this.textStyle = style;
		if (textStyle == TEXT_STYLE_ITALIC) {
			textView.setTypeface(textView.getTypeface(), Typeface.ITALIC);
		} else if (textStyle == TEXT_STYLE_BOLD) {
			textView.setTypeface(textView.getTypeface(), Typeface.BOLD);
		} else {
			textView.setTypeface(textView.getTypeface(), Typeface.NORMAL);
		}
	}

	public int getTextStyle() {
		return textStyle;
	}

	public void setTextColor(int color) {
		this.textColor = color;
		textView.setTextColor(isEnabled ? textColor : disabledTextColor);
	}

	public int getTextColor() {
		return this.textColor;
	}


	public void setBorderWidth(int size) {
		this.borderWidth = size;
		setupBackground();
	}

	public int getBorderWidth() {
		return borderWidth;
	}

	public void setBorderColor(int color) {
		this.borderColor = color;
		setupBackground();
	}

	public int getBorderColor() {
		return this.borderColor;
	}


	public void setRadius(float size) {
		this.radius = size;
		setupBackground();
	}

	public float getRadius() {
		return radius;
	}

	public void setBackgroundColor(@ColorInt int color) {
		this.backgroundColor = color;
		setupBackground();
	}

	public int getBackgroundColor() {
		return this.backgroundColor;
	}

	public void setFocusColor(@ColorInt int color) {
		this.focusColor = color;
		setupBackground();
	}

	public int getFocusColor() {
		return this.focusColor;
	}

	public void setDisableColor(@ColorInt int color) {
		this.disableColor = color;
		setupBackground();
	}

	public int getDisableColor() {
		return disableColor;
	}

	@Override
	public void setEnabled(boolean enabled) {
		//super.setEnabled(enabled);
		this.isEnabled = enabled;
		initializeView();
	}

	public void setDisabledTextColor(int color) {
		this.disabledTextColor = color;
		initializeView();
	}

	public int getDisabledTextColor() {
		return disabledTextColor;
	}

	public void setDisabledColor(int color) {
		this.disableColor = color;
		setupBackground();
	}

	public int getDisabledColor() {
		return disableColor;
	}

	public void setFontIcon(String fontIcon) {
		this.fontIcon = fontIcon;
		imageView.setImageBitmap(getFontBitmap());
	}

	public int getIconSize() {
		return iconSize;
	}

	public void setIconSize(int iconSize) {
		this.iconSize = iconSize;
		imageView.setImageBitmap(getFontBitmap());
	}

	public void setIconColor(int color) {
		this.iconColor = color;
		imageView.setImageBitmap(getFontBitmap());
	}

	public void setIconPosition(int position) {
		this.iconPosition = position;
		initializeView();
	}

	/*public int getIconPosition() {
		return iconPosition;
	}*/

	public void setDrawableResource(@DrawableRes int resource) {
		this.drawableResource = resource;
		//initializeView();
		imageView.setImageResource(resource);
	}

	public void setDrawable(Drawable drawable) {
		this.drawable = drawable;
		initializeView();
	}


	public void setIconPadding(int padding) {
		this.iconPadding = padding;
		//updateIconPadding();
		initializeView();
	}

	public int getIconPadding() {
		return this.iconPadding;
	}

	public void setTextGravity(int gravity) {
		this.lGravity = gravity;
		initializeView();
		//if (textView != null)
		//updateGravity();
		//updateIconPadding();
		/*else
			initializeView();*/
	}


}
