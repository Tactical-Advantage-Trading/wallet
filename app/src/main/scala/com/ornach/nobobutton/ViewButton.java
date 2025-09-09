package com.ornach.nobobutton;

import android.content.Context;
import android.content.res.TypedArray;
import android.graphics.Color;
import android.util.AttributeSet;
import android.widget.LinearLayout;

import androidx.annotation.AttrRes;
import androidx.annotation.NonNull;
import androidx.annotation.Nullable;

import trading.tacticaladvantage.R;

public class ViewButton extends LinearLayout {

	private int backgroundColor = 0;
	private int disableColor = 0;
	private int focusColor = 0xFFCCCCCC;
	private int shape = DrawableHelper.SHAPE_RECTANGLE;
	private int borderWidth = 0;
	private int borderColor = Color.TRANSPARENT;
	private float radius = 0;

	public ViewButton(@NonNull Context context) {
		super(context);
		initializeView();
	}

	public ViewButton(@NonNull Context context, @Nullable AttributeSet attrs) {
		super(context, attrs);
		processAttributes(context, attrs);
		initializeView();
	}

	public ViewButton(@NonNull Context context, @Nullable AttributeSet attrs, @AttrRes int defStyleAttr) {
		super(context, attrs, defStyleAttr);
		processAttributes(context, attrs);
		initializeView();
	}

	private void processAttributes(final Context context, final AttributeSet attrs) {
		/*if (Build.VERSION.SDK_INT < Build.VERSION_CODES.JELLY_BEAN_MR1)
			initDefaultAttributes(attrs);
		else
			initDefaultAttributes17(attrs);*/

		TypedArray attrsArray = context.obtainStyledAttributes(attrs, R.styleable.ViewButton, 0, 0);
		initAttributes(attrsArray);
		attrsArray.recycle();
	}


	private void initAttributes(TypedArray attrs) {

		backgroundColor = attrs.getColor(R.styleable.ViewButton_nb_backgroundColor, backgroundColor);
		disableColor = attrs.getColor(R.styleable.ViewButton_nb_disableColor, disableColor);
		focusColor = attrs.getColor(R.styleable.ViewButton_nb_focusColor, focusColor);
		shape = attrs.getInt(R.styleable.ViewButton_nb_shape, shape);

		borderWidth = attrs.getDimensionPixelSize(R.styleable.ViewButton_nb_borderWidth, borderWidth);
		borderColor = attrs.getInt(R.styleable.ViewButton_nb_borderColor, borderColor);

		radius = attrs.getDimension(R.styleable.ViewButton_nb_radius, radius);

	}


	private void initializeView() {
		setClickable(true);

		// setup background
		DrawableHelper helper = new DrawableHelper.Builder()
			  .setBackgroundColor(backgroundColor)
			  .setFocusColor(focusColor)
			  .setShape(shape)
			  .setDisabledColor(disableColor)
			  .setBorderWidth(borderWidth)
			  .setBorderColor(borderColor)
			  .setRadius((int) radius)
			  .build();

		helper.setBackground(this, true);

	}


}
